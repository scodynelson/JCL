package jcl.compiler.old;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.BitVectorStruct;
import jcl.arrays.StringStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.NReverseFunction;
import jcl.compiler.old.functions.XCopyTreeFunction;
import jcl.compiler.old.symbol.DeclarationOld;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.old.symbol.SpecialOperatorOld;
import jcl.compiler.old.symbol.VariableOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.Marker;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import java.util.Vector;
import java.util.regex.Pattern;

public class SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzer.class);

	public static final SymbolStruct LAMBDA = SpecialOperatorOld.LAMBDA_MARKER;
	public static final SymbolStruct MACRO = SpecialOperatorOld.MACRO_MARKER;
	public static final SymbolStruct COMPILE = VariableOld.Compile;
	public static final SymbolStruct LOAD = VariableOld.Load;
	public static final SymbolStruct EVAL = VariableOld.Eval;
	public static final SymbolStruct Defmacro = GlobalPackageStruct.COMMON_LISP.intern("DEFMACRO").getSymbolStruct();
	public static final SymbolStruct COMPILE_TOPLEVEL = KeywordOld.CompileToplevel;
	public static final SymbolStruct LOAD_TOPLEVEL = KeywordOld.LoadToplevel;
	public static final SymbolStruct EXECUTE = KeywordOld.Execute;
	public static final SymbolStruct FUNCTION_MARKER = SpecialOperatorOld.FUNCTION_MARKER;
	private static final StringStruct LAMBDA_ARGLIST_MUNGER_STRING = new StringStruct("LAMBDA_ARGLIST_MUNGER");

	// eval-when processing modes
	private boolean topLevelMode;

	public enum ProcessingMode {

		COMPILE_TOO, NOT_COMPILE_TOO
	}

	private ProcessingMode mode;

	public enum ProcessingAction {

		PROCESS, EVALUATE, DISCARD
	}

	private ProcessingAction action;
	static String nameBreakingRegex = "[^\\p{Alnum}]";
	static Pattern nameBreakingPattern = Pattern.compile(nameBreakingRegex);
	private ListStruct bindings;
	private Stack<Environment> environmentStack;
	private Stack<SymbolStruct> blockStack;
	private int blockCounter;
	private Stack<Stack> tagbodyStack;
	private int iTagbodyCounter;
	private Vector<SymbolStruct> undefinedFunctions;
	private Stack<ListStruct> currentParsedLambdaList;
	private Stack<SymbolStruct> currentLispName;
	private Stack<IdentityHashMap<LispStruct, LispStruct>> dupSetStack;
	private IdentityHashMap<LispStruct, LispStruct> dupSet = null;
	private int bindingsPosition;
	private Vector symbolVector;
	//    private int closureDepth;
	// and association of function names seen and their arglist munging
	// used to handle recursive functions
	private ListStruct currentArgMunger = NullStruct.INSTANCE;

	private void initialize() {
		//create the global environment
		environmentStack = new Stack<>();
		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());
		currentParsedLambdaList = new Stack<>();
		currentParsedLambdaList.push(NullStruct.INSTANCE);
		currentLispName = new Stack<>();
		currentLispName.push(null);

		topLevelMode = true;
		mode = ProcessingMode.NOT_COMPILE_TOO;

		blockStack = new Stack<>();
		tagbodyStack = new Stack<>();
		iTagbodyCounter = 0;
		undefinedFunctions = new Vector<>();
		bindingsPosition = 0;
		symbolVector = null;
//        closureDepth = 0;
		bindings = NullStruct.INSTANCE;
		dupSetStack = new Stack<>();
	}

	public LispStruct funcall(LispStruct form) {
		initialize();
		if (!(form instanceof ListStruct)
				|| !(((ListStruct) form).getFirst() instanceof SymbolStruct)
				|| !(((ListStruct) form).getFirst().equals(SpecialOperatorOld.LAMBDA)
				|| ((ListStruct) form).getFirst().equals(SpecialOperatorOld.MACRO_LAMBDA))) {
			form = new WrapInLambda().funcall(form);
		}
		// make a copy so we can trash it in SA
		form = saMainLoop(form);
		// now setup the closure depths
		form = saSetClosureDepth(form, 0);
		// clear the dup hash map

		// now see if we have any functions still undefined
		Iterator<SymbolStruct> iterator = undefinedFunctions.iterator();
		while (iterator.hasNext()) {
			if (iterator.next().getFunction() != null) {
				// one with a fn, drop it
				iterator.remove();
			}
		}
		// now print out the ones outstanding
		iterator = undefinedFunctions.iterator();
		while (iterator.hasNext()) {
			final SymbolStruct fnName = iterator.next();
			LOGGER.warn("; Warning: no function or macro function defined for ");
			if (fnName.getSymbolPackage() != null) {
				LOGGER.warn("{}::{}", fnName.getSymbolPackage().getName(), fnName.getName());
			} else {
				LOGGER.warn("#:{}", fnName.getName());
			}
		}
		return form;
	}

	private void replace(ListStruct list, final SymbolStruct oldSym, final SymbolStruct newSym) {
		while (!list.equals(NullStruct.INSTANCE)) {
			if (list.getFirst() instanceof ListStruct) {
				replace((ListStruct) list.getFirst(), oldSym, newSym);
			} else {
				if (list.getFirst().equals(oldSym)) {
					list.setElement(1, newSym);
				}
			}
			list = list.getRest();
		}
	}

	private LispStruct saMainLoop(LispStruct form) {
		if (form.equals(NullStruct.INSTANCE)) {
			// do nothing
		} else if (form instanceof KeywordSymbolStruct) {
			//do nothing
		} else if (form instanceof SymbolStruct) {
			form = saSymbolStruct((SymbolStruct) form);
		} else if (form instanceof ListStruct) {
			form = saList((ListStruct) form);
		} else if (form instanceof BitVectorStruct) {
			//do nothing
		} else if (form instanceof VectorStruct) {
			form = saMainLoop(transformSimpleVectorToLTV((VectorStruct) form));
		} else if (form instanceof CharacterStruct) {
			//do nothing
		} else if (form instanceof NumberStruct) {
			//do nothing
		} else if (form instanceof CharSequence) {
			//do nothing
		} else if (form instanceof VectorStruct) {
			form = saMainLoop(saVectorImpl((VectorStruct) form));
		} else if (form instanceof ArrayStruct) {
			form = saMainLoop(saSimpleArray((ArrayStruct) form));
		} else {
			LOGGER.warn("SA: Found thing I can't semantically analyze: {}, class: {}", form, form.getClass().getName());
		}
		return form;
	}

	private int saResetBindingSlots(final ListStruct theEnv, int depth) {
		// get the binding list from the environment
		ListStruct bindingList = AssocFunction.funcall(KeywordOld.Bindings, theEnv.getRest()).getRest();
		// now run through them and incrementing the depth
		while (!bindingList.equals(NullStruct.INSTANCE)) {
			// get the :allocation parameter - (:LOCAL . n)
			ListStruct bindingElement = (ListStruct) bindingList.getFirst();
			// (x :scope ... :allocation ...)
			bindingElement = bindingElement.getRest();
			final ConsStruct lclSlot = (ConsStruct) GetPlist.funcall(bindingElement,
					KeywordOld.Allocation);
			lclSlot.setCdr(new IntegerStruct(BigInteger.valueOf(depth++)));
			bindingList = bindingList.getRest();
		}
		return depth;
	}

	private int saResetFreeSlots(final ListStruct theEnv, int depth) {
		// get the SymbolStruct table list from the environment
		ListStruct symbolTable = AssocFunction.funcall(KeywordOld.SymbolTable, theEnv.getRest()).getRest();
		// now run through them and incrementing the depth
		while (!symbolTable.equals(NullStruct.INSTANCE)) {
			// get the :allocation parameter - (:LOCAL . n)
			ListStruct tableElement = (ListStruct) symbolTable.getFirst();
			// (x :scope ... :allocation ...)
			tableElement = tableElement.getRest();
			final ConsStruct lclSlot = (ConsStruct) GetPlist.funcall(tableElement,
					KeywordOld.Allocation);
			// have to make sure it's allocated locally
			if (lclSlot.getFirst().equals(KeywordOld.Local)) {
				lclSlot.setCdr(new IntegerStruct(BigInteger.valueOf(depth++)));
			}
			symbolTable = symbolTable.getRest();
		}
		return depth;
	}

	private Object saResetLocals(final Object form, int depth) {
		// go through all forms looking for bindings and reseting the
		// local slot allocations. On encountering a %lambda, the
		// counter is reset to 1 (first parameter). The sequence is
		// to reallocate lexical bindings, then reset the allocations for
		// special VariableOlds
		if (!form.equals(NullStruct.INSTANCE)) {
			if (form instanceof ListStruct) {
				final ListStruct workingList = (ListStruct) form;
				// this may be the start of a lambda or let expr
				// maybe ((system::%lambda (:parent ...) (:bindings...) ...) ...)
				final Object theCar = workingList.getFirst();
				if (theCar instanceof ListStruct) {
					// (system::%lambda (:parent ...) (:bindings...) ...)
					final Object test = ((ListStruct) theCar).getFirst();
					if (test.equals(LAMBDA) || test.equals(SpecialOperator.LET) || test.equals(MACRO)) {
						if (!test.equals(SpecialOperator.LET)) {
							//reset the counter
							depth = 1;
						}
						final ListStruct theEnv = (ListStruct) theCar;
						depth = saResetBindingSlots(theEnv, depth);
						depth = saResetFreeSlots(theEnv, depth);
					}
					saResetLocals(workingList.getRest(), depth + 20); // just in case there are locals allocated
				} else {
					saResetLocals(theCar, depth);
					saResetLocals(workingList.getRest(), depth);
				}
			}
		}
		return form;
	}

	private LispStruct saSetClosureDepth(final LispStruct form, int depth) {
		if (!form.equals(NullStruct.INSTANCE)) {
			if (form instanceof ListStruct) {
				final ListStruct workingList = (ListStruct) form;
				// this may be the start of a lambda or let expr
				final LispStruct theCar = workingList.getFirst();
				if (theCar instanceof ListStruct) {
					final Object test = ((ListStruct) theCar).getFirst();
					if (test.equals(LAMBDA) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.LABELS) || test.equals(SpecialOperator.FLET) || test.equals(MACRO)) {
						final ListStruct theEnv = (ListStruct) theCar;
						// it is, so see if there's a closure defined
						// Get the current closure entry
						final ConsStruct closure = (ConsStruct) AssocFunction.funcall(
								KeywordOld.Closure, theEnv.getRest());
						// add the depth indicator
						// (rplacd closure (cons (cons :depth depth) (cdr closure)))
						// there may be a depth gauge...
						final ListStruct depthGauge = AssocFunction.funcall(KeywordOld.Depth, closure.getRest());
						if (depthGauge.equals(NullStruct.INSTANCE)) { // it may have been handled as a labels
							if (!closure.getRest().equals(NullStruct.INSTANCE)) {
								depth++;
							}
							closure.setCdr(
									new ConsStruct(
											new ConsStruct(
													KeywordOld.Depth, new IntegerStruct(BigInteger.valueOf(depth))),
											closure.getCdr()));
						}
						// walk the body of the lambda or let
						saSetClosureDepth(workingList.getRest(), depth);
					} else {
						saSetClosureDepth(theCar, depth);
						saSetClosureDepth(workingList.getRest(), depth);
					}
				} else {
					if (!theCar.equals(SpecialOperatorOld.DECLARE)) {
						saSetClosureDepth(workingList.getRest(), depth);
					}
				}
			}
		}
		return form;
	}

	private SymbolStruct saSymbolStruct(final SymbolStruct sym) {
		// new way //
		// if SymbolStruct is not bound, then add to the SymbolStruct table and closure
		// Step 1: see if the SymbolStruct is bound in the local environment
		// If so, nothing else to do
		EnvironmentAccessor.addSymbolToTable(environmentStack.peek(), sym);
		return sym;
	}

	private LispStruct saList(ListStruct list) {
		LispStruct retVal = list;
		if (list.getFirst() instanceof SymbolStruct) {
			final SymbolStruct fnName = (SymbolStruct) list.getFirst();
			// ex (append foo bar)
			retVal = (LispStruct) ((Object[]) MacroExpandFunction.FUNCTION.funcall(list, environmentStack.peek()))[0];
			if (retVal.equals(NullStruct.INSTANCE)) {
				return NullStruct.INSTANCE;
			}
			if (retVal instanceof ListStruct) {
				list = (ListStruct) retVal;
				final SymbolStruct sym = (SymbolStruct) list.getFirst();
				if (list.getFirst() instanceof SpecialOperatorOld) {
//                    if (list.getFirst() == SpecialOperatorOld.LABELS) {
//                        list = saFletLabels(list);
//                    } else if (list.getFirst() == SpecialOperatorOld.FLET) {
////                        list = saFlet(list);
//                    }
					retVal = saSpecialOp(list);
				} else {
					retVal = saFunctionCall(list);
				}
			} else {
				// in case the macro expansion didn't result in a list form
				retVal = saMainLoop(retVal);
			}
		} else if (list.getFirst() instanceof ListStruct) {
			// ex ((lambda (x) (+ x 1)) 3)
			final ListStruct first = (ListStruct) list.getFirst();
			if (first.getFirst().equals(SpecialOperatorOld.LAMBDA)) {
				list.setElement(1, saLambda(first));
				retVal = saFunctionCall(list);
			} else {
				final String longStr = list.toString();
				final String str = "Improperly Formed ListStruct -> "
						+ longStr.substring(0, (longStr.length() > 100) ? 100 : longStr.length());
				throw new RuntimeException(str);
			}
		} else {
			final String longStr = list.toString();
			final String str = "Improperly Formed ListStruct -> "
					+ longStr.substring(0, (longStr.length() > 100) ? 100 : longStr.length());
			throw new RuntimeException(str);
		}
		return retVal;
	}

	private ListStruct saVectorImpl(final VectorStruct formVector) {
		ListStruct formList = NullStruct.INSTANCE;
		for (int i = 0; i < formVector.getTotalSize(); i++) {
			formList = new ConsStruct(formVector.getElementAt(i), formList);
		}
		formList = NReverseFunction.funcall(formList);

		final LispStruct adjustableBoolean;
		if (formVector.isAdjustable()) {
			adjustableBoolean = TStruct.INSTANCE;
		} else {
			adjustableBoolean = NILStruct.INSTANCE;
		}

		// (system::%make-array)
		ListStruct functionMakeArray = ListStruct.buildProperList(new SymbolStruct("%MAKE-ARRAY", GlobalPackageStruct.SYSTEM));
		// (function system::%make-array)
		functionMakeArray = new ConsStruct(SpecialOperatorOld.FUNCTION, functionMakeArray);

		// ()
		ListStruct finalList = NullStruct.INSTANCE;
		// (fillPointerIndex)
		if (formVector.getFillPointer() != null) {
			finalList = new ConsStruct(new IntegerStruct(BigInteger.valueOf(formVector.getFillPointer())), finalList);
		}
		// ((formList) fillPointerIndex)
		finalList = new ConsStruct(adjustableBoolean, finalList);
		// ((formList))
		finalList = new ConsStruct(formList, finalList);
		// (nil (formList))
		finalList = new ConsStruct(NILStruct.INSTANCE, finalList);
		// (t nil (formList))
		finalList = new ConsStruct(TStruct.INSTANCE, finalList);
		// (((dimensions) t nil (formList)))
		finalList = ListStruct.buildProperList(new ConsStruct(ListStruct.buildProperList(formVector.getDimensions()), finalList));
		// ((quote ((dimensions) t nil (formList))))
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperatorOld.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct(), finalList));
		return new ConsStruct(SpecialOperatorOld.LOAD_TIME_VALUE, finalList);
	}

	// ********** START OF saSimpleArray() TRANSLATOR ********** //
	private ListStruct saSimpleArray(final ArrayStruct formArray) {
		final ListStruct formList = createSimpleArrayInitialContentsList(formArray);

		// (system::%make-array)
		ListStruct functionMakeArray = ListStruct.buildProperList(new SymbolStruct("%MAKE-ARRAY", GlobalPackageStruct.SYSTEM));
		// (function system::%make-array)
		functionMakeArray = new ConsStruct(SpecialOperatorOld.FUNCTION, functionMakeArray);

		// ((formList))
		ListStruct finalList = ListStruct.buildProperList(formList);
		// (nil (formList))
		finalList = new ConsStruct(NILStruct.INSTANCE, finalList);
		// (t nil (formList))
		finalList = new ConsStruct(TStruct.INSTANCE, finalList);
		// (((dimensions) t nil (formList)))
		if (formArray.getRank() == 0) {
			finalList = ListStruct.buildProperList(new ConsStruct(NullStruct.INSTANCE, finalList));
		} else {
			finalList = ListStruct.buildProperList(new ConsStruct(ListStruct.buildProperList(formArray.getDimensions()), finalList));
		}
		// ((quote ((dimensions) t nil (formList))))
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperatorOld.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct(), finalList));
		return new ConsStruct(SpecialOperatorOld.LOAD_TIME_VALUE, finalList);
	}

	private ListStruct createSimpleArrayInitialContentsList(final ArrayStruct formArray) {
		ListStruct formList = NullStruct.INSTANCE;

		for (int i = 0; i < formArray.getTotalSize(); i++) {
			formList = new ConsStruct(formArray.getElementAt(i), formList);
		}
		formList = NReverseFunction.funcall(formList);
		if ((formArray.getRank() > 1) && !formList.equals(NullStruct.INSTANCE)) {
			formList = initialContentsBuilder(formList, ListStruct.buildProperList(formArray.getDimensions()));
		}
		return formList;
	}

	private ListStruct initialContentsBuilder(ListStruct formList, final ListStruct dimensionsList) {
		ListStruct newList = NullStruct.INSTANCE;

		if (dimensionsList.size() > 1) {
			for (int i = 0; i < ((IntegerStruct) dimensionsList.getFirst()).getBigInteger().intValue(); i++) {
				newList = new ConsStruct(initialContentsBuilder(subListFinder(formList, dimensionsList.getRest()), dimensionsList.getRest()), newList);
				formList = reduceListSize(formList, dimensionsList.getRest());
			}
			newList = NReverseFunction.funcall(newList);
		} else {
			newList = formList;
		}
		return newList;
	}

	private ListStruct subListFinder(ListStruct formList, final ListStruct dimensionsList) {
		ListStruct newList = NullStruct.INSTANCE;

		int sizeOfSubList = 1;
		final Object[] dimensionsAsArray = dimensionsList.getAsJavaList().toArray();
		for (int i = 0; i < dimensionsList.size(); i++) {
			sizeOfSubList = sizeOfSubList * ((IntegerStruct) dimensionsAsArray[i]).getBigInteger().intValue();
		}
		for (int j = 0; j < sizeOfSubList; j++) {
			newList = new ConsStruct(formList.getFirst(), newList);
			formList = formList.getRest();
		}

		return NReverseFunction.funcall(newList);
	}

	private ListStruct reduceListSize(ListStruct formList, final ListStruct dimensionsList) {

		int amountToReduce = 1;
		final Object[] dimensionsAsArray = dimensionsList.getAsJavaList().toArray();
		for (int i = 0; i < dimensionsList.size(); i++) {
			amountToReduce *= ((IntegerStruct) dimensionsAsArray[i]).getBigInteger().intValue();
		}
		for (int j = 0; j < amountToReduce; j++) {
			formList = formList.getRest();
		}

		return formList;
	}
	// ********** END OF saSimpleArray() TRANSLATOR ********** //

	public static FunctionStruct LAMBDA_ARGLIST_MUNGER =
			new FunctionStruct() {

				public LispStruct funcall(final LispStruct arglist, final LispStruct marker) {
					return new ConsStruct(SpecialOperatorOld.FUNCTION_MARKER,
							new ConsStruct(marker, arglist));
				}

				@Override
				public LispStruct apply(final LispStruct... lispStructs) {
					return funcall(lispStructs[0], lispStructs[1]);
				}
			};

	private ListStruct saFunctionCall(ListStruct list) {
		ListStruct cpy = list;
		// check to see if there's a function binding in existence
		if (list.getFirst() instanceof SymbolStruct) {
			SymbolStruct fnName = (SymbolStruct) list.getFirst();
			//TODO make this active only during compiling, not during eval during compile-file
			// handle messaging the argument list according to the lambda list
			// if the car of the function is a special marker, drop the marker
			if (fnName.equals(FUNCTION_MARKER)) {
				// drop the marker
				list = list.getRest();
				cpy = list;
			} else { // not already munged
				final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(environmentStack.peek(), fnName, false);
				if (!fnBinding.equals(Environment.NULL)) {
					// use assoc to get bindings
					final FunctionBinding fooBinding = (FunctionBinding) fnBinding.getBinding(fnName);
					// see if this is a LABELS or FLET. That changes fnName
					fnName = fooBinding.getName();
					list.setElement(1, fnName);
					currentLispName.push(fnName);
					return saFunctionCall(list);
				}
				final ListStruct args = list.getRest();
				final FunctionStruct fnApplying = fnName.getFunction(); // (fnApplying ....)
				if ((fnApplying == null) && undefinedFunctions.contains(fnName)) {
					// add this as a possible undefined function
					undefinedFunctions.add(fnName);
				}
				// NOW - whew...
				// Get the application munger for this function
				// If there is a function, get the munger from the function.
				// If not, use the default one that comes with the FunctionBaseClass
				FunctionStruct munger = LAMBDA_ARGLIST_MUNGER;
				try {
					if ((fnApplying == null) && currentLispName.peek().equals(fnName)) {
						// the function is not known at this point but
						// this is a recursive call and there will be a munger already created
						final ListStruct inProcessMunger = AssocFunction.funcall(fnName, currentArgMunger);
						if (!inProcessMunger.equals(NullStruct.INSTANCE)) {
							munger = (FunctionStruct) ((ConsStruct) inProcessMunger).getCdr();
						}
						currentLispName.pop();
					} else if (fnApplying != null) {
						// here we know that the function was compiled and has a munger
						munger = (FunctionStruct) fnApplying.getClass().getField(LAMBDA_ARGLIST_MUNGER_STRING.getAsJavaString()).get(null);
					} // else gets the default munger
					assert munger != null;
				} catch (final Exception ex) {
					throw new RuntimeException(
							"Unable to get munging function from fn " + fnApplying, ex);
				}
				list = (ListStruct) munger.apply(list.getRest(), list.getFirst());
				return (ListStruct) saMainLoop(list);
			}
		}
		list = list.getRest();
		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		// Now it's possible that this is a recursive call to the current function
		// if so, it has to tagged as such to the icg

		if (cpy.getFirst().equals(currentLispName.peek())) {
			cpy = new ConsStruct(SpecialOperatorOld.TAIL_RECURSION, cpy);
		}
		return cpy;
	}

	private LispStruct saSpecialOp(final ListStruct list) {
		final SymbolStruct symName = (SymbolStruct) list.getFirst();
		LispStruct result = null;

		// Determine the special form and generate its code.
		if (symName.equals(SpecialOperatorOld.BLOCK)) {
			result = saBlock(list);
		} else if (symName.equals(SpecialOperatorOld.CATCH)) {
			result = saCatch(list);
		} else if (symName.equals(SpecialOperatorOld.DECLARE)) {
			result = saDeclare(list);
		} else if (symName.equals(SpecialOperatorOld.DEFSTRUCT)) {
			result = saDefstruct(list);
		} else if (symName.equals(SpecialOperatorOld.EVAL_WHEN)) {
			result = saEvalWhen(list);
		} else if (symName.equals(SpecialOperatorOld.FLET)) {
			result = saFlet(list);
			result = saProgn((ListStruct) result);
		} else if (symName.equals(SpecialOperatorOld.FUNCTION)) {
			result = saFunction(list);
		} else if (symName.equals(SpecialOperatorOld.FUNCTION_MARKER)) {
			result = saFunctionCall(list);
		} else if (symName.equals(SpecialOperatorOld.GO)) {
			result = saGo(list);
		} else if (symName.equals(SpecialOperatorOld.IF)) {
			result = saIf(list);
		} else if (symName.equals(SpecialOperatorOld.LAMBDA)) {
			result = saLambda(list);
		} else if (symName.equals(SpecialOperatorOld.MACRO_LAMBDA)) {
			result = saMacroLambda(list);
		} else if (symName.equals(SpecialOperatorOld.LABELS)) {
			result = saLabels(list);
			result = saProgn((ListStruct) result);
		} else if (symName.equals(SpecialOperatorOld.LET)) {
			result = saLet(list);
		} else if (symName.equals(SpecialOperatorOld.LET_STAR)) {
			result = saLetStar(list);
		} else if (symName.equals(SpecialOperatorOld.LOAD_TIME_VALUE)) {
			result = saLoadTimeValue(list);
		} else if (symName.equals(SpecialOperatorOld.LOCALLY)) {
			result = saLocally(list);
		} else if (symName.equals(SpecialOperatorOld.MACROLET)) {
			result = saMacrolet(list);
		} else if (symName.equals(SpecialOperatorOld.MULTIPLE_VALUE_CALL)) {
			result = saMultipleValueCall(list);
		} else if (symName.equals(SpecialOperatorOld.MULTIPLE_VALUE_PROG1)) {
			result = saMultipleValueProg1(list);
		} else if (symName.equals(SpecialOperatorOld.PROGN)) {
			result = saProgn(list);
		} else if (symName.equals(SpecialOperatorOld.PROGV)) {
			result = saProgv(list);
		} else if (symName.equals(SpecialOperatorOld.QUOTE)) {
			result = saQuote(list);
		} else if (symName.equals(SpecialOperatorOld.RETURN_FROM)) {
			result = saReturnFrom(list);
		} else if (symName.equals(SpecialOperatorOld.SETQ)) {
			result = saSetq(list);
		} else if (symName.equals(SpecialOperatorOld.STATIC_FIELD)) {
			result = saStaticField(list);
		} else if (symName.equals(SpecialOperatorOld.SYMBOL_MACROLET)) {
			result = saSymbolMacrolet(list);
		} else if (symName.equals(SpecialOperatorOld.TAGBODY)) {
			result = saTagbody(list);
		} else if (symName.equals(SpecialOperatorOld.THE)) {
			result = saThe(list);
		} else if (symName.equals(SpecialOperatorOld.THROW)) {
			result = saThrow(list);
		} else if (symName.equals(SpecialOperatorOld.UNWIND_PROTECT)) {
			result = saUnwindProtect(list);
		}
		return result;
	}

	private ListStruct saBlock(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();

		if (!(list.getFirst() instanceof SymbolStruct)) {
			throw new RuntimeException("Block without name: " + list);
		}
		final SymbolStruct label = (SymbolStruct) list.getFirst();
		blockStack.push(label);
		list = list.getRest();

		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		blockStack.pop();
		return cpy;
	}

	private ListStruct saCatch(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();

		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		return cpy;
	}

	// insert from LinL?

	/**
	 * This method parses the declaration specifications - which may also
	 * hold a documentation string. All of the declarations are merged into
	 * a single DECLARE form. The doc string becomes another DECLARE option.
	 * The BNF is
	 * declarations := declSpec* docString? declSpec*
	 * declSpec := '(' DECLARE declaration* ')'
	 * declaration := '(' declName declArgs ')'
	 * <p>
	 * If there is nothing after the string, then the string is the form
	 * Not the doc string
	 */
	private ListStruct saDeclarations(ListStruct list) {
		// list is the rest of the forms after the arg list
		ListStruct newDecls = NullStruct.INSTANCE;   //the list of all new Declarations
		final ListStruct returnList = list;

		while (!list.equals(NullStruct.INSTANCE)) {
			LispStruct theCar = list.getFirst();
			ListStruct theDecl;    //the current new DeclarationOld
			if (theCar instanceof CharSequence) {
				// This may be a doc string, unless it's the last thing
				if (list.getRest().equals(NullStruct.INSTANCE)) {
					break;
				} else {
					// there's stuff after the doc string so it is doc
					theDecl = ListStruct.buildProperList(ListStruct.buildProperList(DeclarationOld.DOCUMENTATION, theCar));
					newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
					list = list.getRest();

					// add code to look for more declarations - without any strings
					theCar = list.getFirst();
					while ((theCar instanceof ListStruct)
							&& ((ListStruct) theCar).getFirst().equals(SpecialOperatorOld.DECLARE)) {
						theDecl = ((ListStruct) theCar).getRest();
						newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
						list = list.getRest();
						theCar = list.getFirst();
					}
					// saDeclarationsHelp will complete the search for declarations
					break;
				}
			}
			if ((theCar instanceof ListStruct) && ((ListStruct) theCar).getFirst().equals(SpecialOperatorOld.DECLARE)) {
				theDecl = ((ListStruct) theCar).getRest();
				newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
			} else {
				break;
			}
			list = list.getRest();
		}

		// if there isn't a DeclarationOld.JAVA_CLASS_NAME, add one
		final ListStruct classNameDecl = AssocFunction.funcall(DeclarationOld.JAVA_CLASS_NAME, newDecls);
		if (classNameDecl.equals(NullStruct.INSTANCE)) {
			final SymbolStruct classname;
			// if there's a Lisp Name, then Javafy it
			final ListStruct lispNameDecl = AssocFunction.funcall(DeclarationOld.LISP_NAME, newDecls);
			if (lispNameDecl.equals(NullStruct.INSTANCE)) {
				currentLispName.push(null);
				final String name = "AnonymousLambda_" + System.currentTimeMillis() + '_';
				classname = (SymbolStruct) GensymFunction.funcall(name);
			} else {
				final SymbolStruct lispName = (SymbolStruct) lispNameDecl.getRest().getFirst();
				classname = (SymbolStruct) GensymFunction.funcall(javafy(lispName) + System.currentTimeMillis());
				// this code allows saFunctionCall to recognize a recursive call
				// the pop happens at the end of saLambda
				currentLispName.push(lispName);
			}
			newDecls = new ConsStruct(new ConsStruct(DeclarationOld.JAVA_CLASS_NAME, ListStruct.buildProperList(classname)), newDecls);
		}
		currentLispName.push(null);
		// if the value is a String, make it into a SymbolStruct
		final Object name = getCadr(classNameDecl);
		if (name instanceof CharSequence) {
			final ListStruct rest = classNameDecl.getRest();
			rest.setElement(1, new SymbolStruct(name.toString()));
		}
		// now we conjure a new declaration
		return new ConsStruct(new ConsStruct(SpecialOperatorOld.DECLARE, newDecls), list);
	}

	// routine to javafy a Lisp SymbolStruct or string to a Java identifier
	private String javafy(final Object lispName) {
		final StringBuilder result = new StringBuilder();
		final Scanner scanner = new Scanner(lispName.toString()).useDelimiter(nameBreakingPattern);

		while (scanner.hasNext()) {
			String fragment = scanner.next();
			if ((fragment == null) || fragment.isEmpty()) {
				fragment = "__";
			}
			final int codePoint = Character.toUpperCase(fragment.codePointAt(0));
			result.appendCodePoint(codePoint);
			result.append(fragment.substring(1).toLowerCase());
		}
		if (result.length() == 0) {
			result.append("UnknownLispName");
		}
		return result.toString();
	}

	private ListStruct findDeclaration(final SymbolStruct item, final ListStruct list) {
		return AssocFunction.funcall(item, list);
	}

	private ListStruct saDeclare(final ListStruct list) {
		SymbolStruct sym;

		// there may not be any declarations
		if (list.getRest().equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		final List<LispStruct> javaRestList = list.getRest().getAsJavaList();
		for (final LispStruct nextDecl : javaRestList) {
			final ListStruct declarationSpec = (ListStruct) nextDecl;
			final Object declIdentifier = declarationSpec.getFirst();
			final ListStruct declList = declarationSpec.getRest();

			// now come the various cases
			if (declIdentifier.equals(DeclarationOld.LISP_NAME)) {
				saLispNameDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.JAVA_CLASS_NAME)) {
				saJavaNameDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.NO_GENERATE_ANALYZER)) {
				saNoGenerateAnalyzerDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.PARSED_LAMBDA_LIST)) {
				saParsedLambdaListDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.DOCUMENTATION)) {
				saDocumentationDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.SOURCE_FILE)) {
				saSourceFileDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.DECLARATION)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.DYNAMIC_EXTENT)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.FTYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.IGNORABLE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.IGNORE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.INLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.NOTINLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.OPTIMIZE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationOld.SPECIAL)) {
				saSpecialDeclaration(declList);
			} else if (declIdentifier.equals(DeclarationOld.TYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(NullStruct.INSTANCE)) {
				//drop it on the floor
			} else {
				LOGGER.warn("DECLARE: unknown specifier: {}", declIdentifier);
			}
		}
		return list;
	}

	/**
	 * Handles the declaration of the Lisp name (SymbolStruct) of the function
	 */
	private void saLispNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private void saJavaNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private void saNoGenerateAnalyzerDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the parsed lambda list of the function
	 */
	private void saParsedLambdaListDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private void saDocumentationDeclaration(final ListStruct declarationList) {
	}

	private void saSourceFileDeclaration(final ListStruct declarationList) {
	}

	/**
	 * handle the components of a Special declaration
	 */
	private void saSpecialDeclaration(final ListStruct declarations) {
		final List<LispStruct> declaractionsJavaList = declarations.getAsJavaList();
		SymbolStruct sym = null;
		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct nextDecl : declaractionsJavaList) {
			try {
				saSymbolStruct(sym = (SymbolStruct) nextDecl);
			} catch (final ClassCastException ccExcption) {
				LOGGER.error("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: {}", sym);
			}
		}
	}

	private ListStruct saDefstruct(final ListStruct list) {
		return list;
	}

	private LispStruct saEvalWhen(final ListStruct list) {
		// this only has effect when the :execute situation is found
		// :compile-toplevel and :load-toplevel situations are ignored.
		// for any of the nested code to be compiled, the :execute
		// situation is in effect.
		if (((ListStruct) list.getRest().getFirst()).getAsJavaList().contains(EXECUTE)) {
			final LispStruct firstElement = list.getFirst();
			final ListStruct firstRest = list.getRest();

			final LispStruct secondElement = firstRest.getFirst();
			final ListStruct secondRest = firstRest.getRest();

			final LispStruct foo = ListStruct.buildProperList(firstElement, secondElement, SpecialOperatorOld.PROGN, secondRest);
			return saMainLoop(foo);
		} else {
			return NullStruct.INSTANCE;
		}
	}


	/**
	 * LABELS and FLET<p>
	 * This is the general algorithm for binding a named function. The issue is binding the name
	 * (a SymbolStruct) by changing the function slot. The process is similar to binding the value (dynamic)
	 * slot of a SymbolStruct - , for example, (let ((a 15)) (declare (special a)) ...a..). Inside the LET
	 * form, the 'a' SymbolStruct's value is 15, but reverts to its value at the end of the LET form.
	 * The LABELS form will be treated as lexical VariableOld forms. If you treat them as special, you
	 * might change a function in a completely function. Not wha you want to have.,p.
	 * <p>
	 * Creating this situation requires coordinated work from the SA and the ICG.<p>
	 * The LABELS form looks like this:<p>
	 * <code>(labels ((named-fn-1 (...lambda-list-1...) body-1) ...) ...forms to evaluate in this env ...)</code><p>
	 * In LABELS, the function binding is visible to the body of all of the named functions as well as
	 * the body of the forms in the LABEL body. This differs from FLET where the named functions
	 * are visible to the FLET body but not the other named functions. In fact, the bodies of the named
	 * FLET functions are not aware of the named function bindings.
	 *
	 * @param list
	 * @return the result of evaluating the forms
	 */
	private ListStruct saLabels(final ListStruct list) {  // return NullStruct.INSTANCE; }

		final SymbolStruct labelsSymbolStruct = (SymbolStruct) list.getFirst();
		final FunctionStruct findLabelsFn;

		final PackageSymbolStruct pOLL = GlobalPackageStruct.COMPILER.findSymbol("FIND-LABELS");
		if (pOLL.getPackageSymbolType() != null) {
			findLabelsFn = pOLL.getSymbolStruct().getFunction();
		} else {
			LOGGER.warn("Unable to obtain the FIND-LABELS Function, LABELS cannot work in this configuration.");
			return NullStruct.INSTANCE;
		}

		final ListStruct theMungedForm;
		if (labelsSymbolStruct.equals(SpecialOperatorOld.LABELS)) {
			theMungedForm = (ListStruct) findLabelsFn.apply(list);
		} else {
			return NullStruct.INSTANCE;
		}
		// this creates '(progn ...mumble). So the list needs to be melded into the
		// the form like the other forms
		list.setElement(1, SpecialOperatorOld.PROGN);
		((ConsStruct) list).setCdr(theMungedForm);
		return list;
	}
/*
(defun find-labels (list)
  (if (and list
          (listp list)
          (eq (car list) 'labels)
          (rest list)
          (cadr list))
    (let* ((the-fns (cadr list))   ; ((foo ()...) (bar ()...))
           (renaming-list
             (system::%mapcan
               #'(lambda (def)
                   (list (car def) (intern (string (gensym (car def))) (symbol-package (car def)))))
               the-fns))           ; (foo foo.. bar bar...)
           (munged-fns nil))

      ;; this gives the local fns
      (setq munged-fns
        (system::%mapcar           ; name, params, body
          #'(lambda (n-p-and-b)    ; swap names
              (cons (system::%get-plist renaming-list (car n-p-and-b) nil)
                (cons (cadr n-p-and-b) (rename-function (cddr n-p-and-b) renaming-list))))
          the-fns))

      (setq munged-fns
        (system::%mapcar
          #'(lambda (form)
              (let ((the-name (car form))
                    (the-lambda-list (cadr form))
                    (the-body (cddr form)))
                (setq the-body `(block ,the-name ,@the-body))
                `(if (not (fboundp ',the-name))
                   (cl::set-symbol-function ',the-name #'(lambda ,the-lambda-list ,the-body)))))
          munged-fns))

      ;; this gives the munged body
      (let ((the-body (cddr list)))
        (nconc munged-fns  (rename-function the-body renaming-list))))))
 */

	private ListStruct saFlet(final ListStruct list) {  // return NullStruct.INSTANCE; }
		final SymbolStruct fletSymbolStruct = (SymbolStruct) list.getFirst();
		final FunctionStruct findFletFn;

		final PackageSymbolStruct pOLL = GlobalPackageStruct.COMPILER.findSymbol("FIND-FLET");
		if (pOLL.getPackageSymbolType() != null) {
			findFletFn = pOLL.getSymbolStruct().getFunction();
		} else {
			LOGGER.warn("Unable to obtain the FIND-FLET Function, FLET cannot work in this configuration.");
			return NullStruct.INSTANCE;
		}

		final ListStruct theMungedForm;
		if (fletSymbolStruct.equals(SpecialOperatorOld.FLET)) {
			theMungedForm = (ListStruct) findFletFn.apply(list);
		} else {
			return NullStruct.INSTANCE;
		}
		// this creates '(progn ...mumble). So the list needs to be melded into the
		// the form like the other forms
		list.setElement(1, SpecialOperatorOld.PROGN);
		((ConsStruct) list).setCdr(theMungedForm);
		return list;
	}
/*
(defun find-flet (list)
  (if (and list
          (listp list)
          (eq (car list) 'flet)
          (rest list)
          (cadr list))
    (let* ((the-fns (cadr list))   ; ((foo ()...) (bar ()...))
           (renaming-list
             (system::%mapcan
               #'(lambda (def)
                   (list (car def) (intern (string (gensym (car def))) (symbol-package (car def)))))
               the-fns))           ; (foo foo.. bar bar...)
           (munged-fns nil))

      ;; this gives the local fns
      (setq munged-fns
        (system::%mapcar           ; name, params, body
          #'(lambda (n-p-and-b)    ; swap names
              (cons (system::%get-plist renaming-list (car n-p-and-b) nil)
                (cons (cadr n-p-and-b) (cddr n-p-and-b))))
          the-fns))

      (setq munged-fns
        (system::%mapcar
          #'(lambda (form)
              (let ((the-name (car form))
                    (the-lambda-list (cadr form))
                    (the-body (cddr form)))
                (setq the-body `(block ,the-name ,@the-body))
                `(if (not (fboundp ',the-name))
                   (cl::set-symbol-function ',the-name #'(lambda ,the-lambda-list ,the-body)))))
          munged-fns))

      ;; this gives the munged body
      (let ((the-body (cddr list)))
        (nconc munged-fns  (rename-function the-body renaming-list))))))
 */

	//**** Need to add FLET ***

	private ListStruct saFunction(ListStruct list) {
		// (function foo) or (function (lambda (...) ..)) or (function (setf foo)), also of course #'
		if (list.size() != 2) {
			throw new RuntimeException("Wrong number of arguments to special operator Function, " + list.size());
		}

		if (!list.getFirst().equals(SpecialOperatorOld.FUNCTION)) {
			throw new RuntimeException("Wrong type of FUNCTION, found " + list.getFirst());
		}

		final LispStruct fnSpec = getCadr(list);

		if (fnSpec instanceof SymbolStruct) {
			SymbolStruct fnSpecAsSymbol = (SymbolStruct) fnSpec;
			final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(environmentStack.peek(), fnSpecAsSymbol, false);
			if (fnBinding.equals(Environment.NULL)) {
				saSymbolStruct(fnSpecAsSymbol);
			} else {
				// use assoc to get bindings
				final FunctionBinding fooBinding = (FunctionBinding) fnBinding.getBinding(fnSpecAsSymbol);
				// see if this is a LABELS or FLET. That changes fnName
				fnSpecAsSymbol = fooBinding.getName();
				// change the cadr
				final ListStruct theCdr = list.getRest();
				theCdr.setElement(1, fnSpecAsSymbol);
			}
		} else if (fnSpec instanceof ListStruct) {
			final ListStruct fn = (ListStruct) fnSpec;
			final Object theCar = fn.getFirst();
			if (theCar.equals(SpecialOperatorOld.LAMBDA) || theCar.equals(SpecialOperatorOld.MACRO_LAMBDA)) {
				list = saLambdaAux(fn);
//            } else if ((theCar == SpecialOperatorOld.FUNCTION) &&
//                       ((((ListStruct)fn.getRest().getFirst()).getFirst() == SpecialOperatorOld.LAMBDA) ||
//                        (((ListStruct)fn.getRest().getFirst()).getFirst() == SpecialOperatorOld.MACRO_LAMBDA))) {
//                list = saLambdaAux((ListStruct)fn.getRest().getFirst());
			} else if (theCar.equals(GlobalPackageStruct.COMMON_LISP.intern("SETF").getSymbolStruct())) {
				return list; // no changes at this level
			} else {
				throw new RuntimeException(
						"Semantic Error in Function, if first arguement is a list it must be a LAMBDA");
			}
		} else {
			throw new RuntimeException(
					"Semantic Error in Function, the arguments must be either a list or SymbolStruct");
		}
		return list;
	}

	private ListStruct saGo(ListStruct list) {
		final ListStruct listCopy = list;

		// Make sure there is only one parameter to GO, and that it is either
		// a SymbolStruct or an integer.
		list = list.getRest();
		final Object tag = list.getFirst();
		if (listCopy.size() < 2) {
			throw new RuntimeException(
					"GO: too few parameters passed to special " + "operator GO: " + listCopy);
		}
		if (listCopy.size() > 2) {
			throw new RuntimeException(
					"GO: too many parameters passed to special " + "operator GO: " + listCopy);
		}
		if (!(tag instanceof SymbolStruct) && !(tag instanceof Number)) {
			throw new RuntimeException("GO: illegal tag: " + tag);
		}

		final SymbolStruct sym = goGetTagSymbolStruct(tag);
		if (sym == null) {
			throw new RuntimeException("GO: no tag named " + tag + " is currently visible");
		}
		list.setElement(1, sym);
		return listCopy;
	}

	/* Search the `tagbodyStack' stack and all its sub-stacks for the specified
	 * tag. If it is found, return the tag's 'new SymbolStruct', else return null. */
	private SymbolStruct goGetTagSymbolStruct(final Object tag) {

		boolean bFound = false;
		SymbolStruct sym = null;

		int i = tagbodyStack.size() - 1;
		while ((i > -1) && !bFound) {
			final Stack stack = tagbodyStack.get(i);

			int j = stack.size() - 1;
			while ((j > -1) && !bFound) {
				final ListStruct list = (ListStruct) stack.get(j);
				final Object obj = list.getFirst();

				if (tag.equals(obj)) {
					sym = (SymbolStruct) ((ConsStruct) list).getCdr();
					bFound = true;
				}
				j--;
			}
			i--;
		}

		return sym;
	}

	private ListStruct saIf(ListStruct list) {
		final ListStruct cpy = list;

		list = list.getRest();
		final int size = list.size();

		if ((size > 3) || (size < 2)) {
			throw new RuntimeException(
					"Incorrect number of arguments, (" + size + ") to IF statement in form " + cpy);
		}
		list.setElement(1, saMainLoop(list.getFirst()));
		list = list.getRest();
		list.setElement(1, saMainLoop(list.getFirst()));
		list = list.getRest();

		if (size != 2) {
			list.setElement(1, saMainLoop(list.getFirst()));
		}
		return cpy;
	}

	private ListStruct saLet(final ListStruct list) {
		SymbolStruct sym;
		ListStruct listValues;
		ListStruct expList;
		ListStruct SymbolStructList;
		ListStruct formList;
		final int i = 0;
		boolean bPastDeclareForms;

		//First see if there are any parameters
		if (list.size() == 1) {
			return NullStruct.INSTANCE;
		}
		// Second see if there's a null binding spec
		if (list.getRest().getFirst().equals(NullStruct.INSTANCE)) {
			// change (let () mumble) to (progn mumble) -- later make it a LOCALLY
//            ListStruct foo = (ListStruct)cons.funcall(SpecialOperatorOld.PROGN, list.getRest().getRest());
//            return saProgn(foo);
			return saProgn(new ConsStruct(SpecialOperatorOld.PROGN, list.getRest().getRest()));
		}
		// Create the list to hold the new lambda expression.
		// --------------------------
		// Semantic Analysis

		// keep track of the current environment as it is "now"
		final Environment prevEnvironment = environmentStack.peek();
		// make a new environment and set it to "current"
		final Environment newLetEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LET);
		// set the current environment's parent to what was the environment
		environmentStack.push(EnvironmentAccessor.createParent(newLetEnvironment, prevEnvironment));
		try {
			// Create the vectors for the SymbolStructs and their values.
			final int tempPosition = bindingsPosition;

			// LET must have at least one parameter.
			ListStruct tmpList = list.getRest();
			// ((...bindings...) ...)
			if (tmpList.size() < 1) {
				throw new RuntimeException("LET: too few parameters for special" + "operator LET: " + list.toString());
			}

			// Now build a list containing all the local VariableOlds for the LET,
			// and a list containing the initial values for the local VariableOlds.
			final ListStruct listSymbolStructs = NullStruct.INSTANCE;

			if (tmpList.getFirst() instanceof ListStruct) {
				ListStruct symList = (ListStruct) tmpList.getFirst();
				// (...bindings...)

				// Loop through the SymbolStructs and store them.
				//first we have to find out the first available open local slot number
				int iNumParams = 0;
				while (symList.size() > 0) {
					if (symList.getFirst() instanceof ListStruct) {
						final ListStruct param = (ListStruct) symList.getFirst();
						// this must be done in the context of the outer environment
						final Environment tmpCurrent = environmentStack.pop();
						final LispStruct initForm = saMainLoop(param.getRest().getFirst());
						environmentStack.push(tmpCurrent);
						bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(tmpCurrent);
						try {
							addBinding_Let((SymbolStruct) param.getFirst(), bindingsPosition, initForm);
						} catch (final Exception ex) {
							LOGGER.error("Exception in SA in addBinding: ");
							LOGGER.error("param: {}", param.getFirst());
							LOGGER.error("bindingsPosition: {}", bindingsPosition);
							LOGGER.error("initForm: {}", initForm);
							LOGGER.error(ex.getMessage(), ex);
						}
					} else {
						bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(environmentStack.peek());
						addBinding_Let((SymbolStruct) symList.getFirst(), bindingsPosition, NullStruct.INSTANCE); //default
					}
					iNumParams++;
					symList = symList.getRest();
				}
				tmpList = tmpList.getRest();

			} else {
				throw new RuntimeException("LET: illegal VariableOld specification");
			}

			ListStruct copyList = tmpList;
			if (tmpList.equals(NullStruct.INSTANCE)) {
				copyList = ListStruct.buildProperList(NullStruct.INSTANCE);
			} else {
				while (!tmpList.equals(NullStruct.INSTANCE)) {
					tmpList.setElement(1, saMainLoop(tmpList.getFirst()));
					tmpList = tmpList.getRest();
				}
			}

			// Now we have to reverse the list of bindings sinec they are pushed on
			final Environment envList = environmentStack.peek();

			bindingsPosition = tempPosition;
			return new ConsStruct(environmentStack.peek(), copyList);
		} finally {
			environmentStack.pop();
		}
	}

	private ListStruct saLetStar(final ListStruct list) {
		// turn a let* into a set of nested LET forms...
		// let* looks like (let* (binding0, binding1, binding2...) all-the-rest)
		// it gets turned into (let (binding0) (let (binding1) (let (binding2) ... all-the-rest)))
		final ListStruct bindingLists = (ListStruct) getCadr(list);
		final ListStruct allTheRest = list.getRest().getRest();
		if (bindingLists.equals(NullStruct.INSTANCE)) {
			list.setElement(1, SpecialOperatorOld.LET);
			return saLet(list);
		}
		final ListStruct theXformedList = letStarHelper(bindingLists, allTheRest);
		return saLet(theXformedList);
	}

	private ListStruct letStarHelper(final ListStruct bindingLists, final ListStruct allTheRest) {
		if (bindingLists.getRest().equals(NullStruct.INSTANCE)) {
			// (cons 'let (cons bindinglist allTheRest))
			return new ConsStruct(SpecialOperatorOld.LET, new ConsStruct(bindingLists, allTheRest));
		} else {
			// (cons 'let (list (list (first bindingLists)) (letStarHelper (rest bindingLists) allTheRest)))
			ListStruct bindingListsCar = NullStruct.INSTANCE;
			if (bindingLists.getFirst() instanceof ListStruct) {
				bindingListsCar = (ListStruct) bindingLists.getFirst();
			} else {
				bindingListsCar = new ConsStruct(bindingLists.getFirst(), bindingListsCar);
			}
			final ListStruct bindingList = ListStruct.buildProperList(bindingListsCar);
			return new ConsStruct(SpecialOperatorOld.LET, new ConsStruct(bindingList,
					ListStruct.buildProperList(letStarHelper(bindingLists.getRest(), allTheRest))));
		}
	}

	// 1. get the current lambda environment
	// 2. get the LTV list
	// 3. wrap the rest of the list in a (lambda () ...)
	// 4. make a new field name
	// 5. make a cons of the name and the lambda into a plist in the environment
	// 6. add the cons to the LTV list
	// 7. return the LTV marker and the field name
	//    (load-time-value 'this is the field name')
	private ListStruct saLoadTimeValue(final ListStruct list) {
		return saLoadTimeValue(list, "LOAD_TIME_VALUE_", GensymFunction.funcall("" + System.currentTimeMillis() + '_').toString());
	}

	private ListStruct saLoadTimeValue(final ListStruct list, final String ltvFieldName) {
		return saLoadTimeValue(list, ltvFieldName, "");
	}

	private ListStruct saLoadTimeValue(final ListStruct list, final String ltvFieldName, final String tag) {
		final Environment lambdaEnv = EnvironmentAccessor.getEnclosingLambda(environmentStack.peek());
		ListStruct ltv = list.getRest();
		// better name this sucker
		final SymbolStruct name = (SymbolStruct) GensymFunction.funcall(ltvFieldName + "_FN_" + tag);
		ListStruct decl = ListStruct.buildProperList(DeclarationOld.JAVA_CLASS_NAME, name);
		decl = ListStruct.buildProperList(SpecialOperatorOld.DECLARE, decl);
		// now it's (declare (%java-class-name "blah"))
		ltv = new ConsStruct(decl, ltv);
		ltv = new ConsStruct(NullStruct.INSTANCE, ltv);
		ltv = new ConsStruct(SpecialOperatorOld.LAMBDA, ltv);
		// now it's (lambda () (declare (%java-class-name "blah")) ..body..)

		// Now we have to recursively munge the lambda, but in the global environment
		try {
			environmentStack.push(environmentStack.elementAt(0));
			// make a form ... ( ((%lambda ...)...) ) - gives room to add the name at front
			ltv = saLambda(ltv);
		} finally {
			environmentStack.pop();
		}
		final SymbolStruct ltvName = new SymbolStruct(ltvFieldName + tag);

		// now it has to be put into the lambda environmment
		final LoadTimeValue ltvAssoc = lambdaEnv.getLoadTimeValue();

		// CDR = (ltvName ltv CDR)
		ltvAssoc.getValues().put(ltvName, ltv);

		return ListStruct.buildProperList(list.getFirst(), ltvName);
	}

	private ListStruct saLocally(final ListStruct list) {
		LOGGER.error("; Warning: LOCALLY not supported at this time.");
		return list;
	}

	private ListStruct saMacrolet(final ListStruct list) {
		// This is a bit simplistic. We turn the local macro definitions into defmacro forms after
		// renaming the local macro names. The environment is like FLET where the pairing of  the
		// original names and new names reside. The body just expands in this environment.
		try {
			final Environment parentEnvironment = environmentStack.peek();
			// make a new environment and set it to "current"
			final Environment newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACROLET);
			final ListStruct fnList = (ListStruct) list.getRest().getFirst();
			// Now has ((foo ...) (bar ...) ...)
			final List<LispStruct> fnListJavaList = fnList.getAsJavaList();
			final ListStruct setForms = NullStruct.INSTANCE;
			for (final LispStruct fnListNext : fnListJavaList) {
				final ListStruct fn = (ListStruct) fnListNext;
				final SymbolStruct newName = GlobalPackageStruct.SYSTEM.intern(javafy(fn.getFirst()) + System.currentTimeMillis()).getSymbolStruct();
				// augment the environment
				final int position = 1; // bindings++ ???
				EnvironmentAccessor.createNewFBinding(newEnvironment, (SymbolStruct) fn.getFirst(), position, newName, false);
				// Now make a new macro form
				// ... (defmacro newName (...) ...body...)
				// hold the rest (lambda list and body)
				ListStruct theRest = fn.getRest();
				// give it a new name
				theRest = new ConsStruct(newName, theRest);
				// call it DEFMACRO
				theRest = new ConsStruct(Defmacro, theRest);
				// since they are eval'ed in a null environment, time to do it
			}
			// now the body is compiled in the FLET environment
			// set the current environment's parent to what was the environment
			environmentStack.push(
					EnvironmentAccessor.createParent(newEnvironment, parentEnvironment));

			ListStruct body = list.getRest().getRest();
			// now we have to handle the body - including declarations
			// sa the body
			final ListStruct copyBody = body;
			while (!body.equals(NullStruct.INSTANCE)) {
				final LispStruct mainStuff = saMainLoop(body.getFirst());
				body.setElement(1, mainStuff);
				body = body.getRest();
			}
			return new ConsStruct(environmentStack.peek(), copyBody);
		} finally {
			environmentStack.pop();
		}
	}

	private ListStruct saMultipleValueCall(ListStruct list) {
		final ListStruct cpy = list;
		final int size = list.size();

		if (size < 2) {
			throw new RuntimeException(
					"MULTIPLE-VALUE-CALL: incorrect number of arguments: " + (size - 1));
		}
		// deal with the function form
		list = list.getRest();
		list.setElement(1, saMainLoop(list.getFirst()));

		// now handle each of the value forms
		while (!(list = list.getRest()).equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
		}
		return cpy;
	}

	private ListStruct saMultipleValueProg1(final ListStruct list) {
		/* This is a bit ugly and conses too much, but someday we'll
		 * have a REAL compiler...
         * It gets turned into...
         * (let ((vals (gensym)))
         *    `(let ((,vals (multiple-value-list ,(second list))))
         *      (progn ,(cddr list) (values-list ,vals)))
         */
		if (list.size() < 2) {
			throw new RuntimeException(
					"MULTIPLE-VALUE-PROG-1: incorrect number of arguments: " + (list.size() - 1));
		}
		final SymbolStruct vals = (SymbolStruct) GensymFunction.funcall();
		// make the progn part
		ListStruct valsList = ListStruct.buildProperList(vals);
		valsList = new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("VALUES-LIST").getSymbolStruct(), valsList);
		//(values-list ,vals)
		valsList = (ListStruct) AppendFunction.funcall(list.getRest().getRest(), ListStruct.buildProperList(valsList));
		//(,(cddr list) (values-list ,vals))
		valsList = new ConsStruct(SpecialOperatorOld.PROGN, valsList);
		// (progn ,(cddr list) (values-list ,vals))

		// build the LET part...
		ListStruct letPiece = ListStruct.buildProperList(list.getRest().getFirst());
		letPiece = new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("MULTIPLE-VALUE-LIST").getSymbolStruct(), letPiece);
		// (multiple-value-list ,(second list))
		letPiece = ListStruct.buildProperList(letPiece);
		// ((multiple-value-list ,(second list)))
		letPiece = new ConsStruct(vals, letPiece);
		// (,vals (multiple-value-list ,(second list)))
		letPiece = ListStruct.buildProperList(letPiece);
		// ((,vals (multiple-value-list ,(second list))))

		// put things together
		valsList = ListStruct.buildProperList(valsList);
		// ((progn ,(cddr list) (values-list ,vals)))
		letPiece = ListStruct.buildProperList(letPiece);
		// ( ((,vals (multiple-value-list ,(second list)))) )
		letPiece = (ListStruct) AppendFunction.funcall(letPiece, valsList);
		// ( ((,vals (multiple-value-list ,(second list)))) (progn ,(cddr list) (values-list ,vals))))
		letPiece = new ConsStruct(SpecialOperatorOld.LET, letPiece);
		// (let ((,vals (multiple-value-list ,(second list)))) (progn ,(cddr list) (values-list ,vals))))
		return (ListStruct) saMainLoop(letPiece);
	}

/*
(defmacro multiple-value-list (&rest forms)
  (declare (system::%java-class-name "lisp.common.function.MultipleValueList"))
  `(multiple-value-call #'list ,@forms)) ;M-V-C is a SpecialOperator...
 */

	private ListStruct saProgn(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();
		if (list.equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		return cpy;
	}

	private ListStruct saProgv(final ListStruct list) {
		LOGGER.error("; Warning: PROGV not supported at this time.");
		return list;
	}

	//******** transforming non-trivial QUOTE forms
	private LispStruct saQuote(final ListStruct list) {
		return saQuote(list, null);
	}

	private LispStruct saQuote(final ListStruct list, final String fldName) {
		if (list.size() != 2) {
			throw new RuntimeException("QUOTE: incorrect number of arguments");
		}
		// special case. (quote (function foo)) => (function foo)
//        Object maybeFunction = list.getRest().getFirst();
//        if ((maybeFunction instanceof ListStruct) && (((ListStruct)maybeFunction).getFirst() == SpecialOperatorOld.FUNCTION)) {
//            return saMainLoop(maybeFunction);
//        }

		// don't make up a lot of load-time-value stuff for constants
		final LispStruct quotedElt = list.getRest().getFirst();
		if ((quotedElt instanceof NumberStruct)
				|| (quotedElt instanceof CharacterStruct)
				|| (quotedElt instanceof StringStruct)
				|| (quotedElt instanceof CharSequence)) {
			return quotedElt;
		}

		if ((quotedElt instanceof SymbolStruct) && (((SymbolStruct) quotedElt).getSymbolPackage() != null)) {
			return list; // handled in icg
		}

		// Now, it's possible that we've seen this element before
		// If so, we've already made up a LOAD-TIME-VALUE form. No
		// need to make another...
		final LispStruct previousElt = dupSet.get(quotedElt);
		if (previousElt instanceof SymbolStruct) {
			// the SymbolStruct is really the name of an LTV value
			return ListStruct.buildProperList(SpecialOperatorOld.LOAD_TIME_VALUE, previousElt);
			// the icg will turn it into a field access instruction
		}

		// The rest of this code creates a LET form (possibly with no args).
		// The LET body consists of code to re-create the original form that
		// was quoted in the source code. Many of the components are made up
		// with load-time-value forms

		//*** NOTE ***
		// For now, the code does NOT handle multiple instances of uninterned
		// SymbolStructs and does not handle circular lists. This awaits the new
		// Lisp compiler (easier to handle this stuff)

		final ListStruct initForm = (ListStruct) transformQuoteToLTV(quotedElt);
		ListStruct newList = NullStruct.INSTANCE;
		newList = new ConsStruct(initForm, newList);
		newList = new ConsStruct(SpecialOperatorOld.LOAD_TIME_VALUE, newList);
		return (fldName == null) ? saLoadTimeValue(newList) : saLoadTimeValue(newList, fldName);
	}

	private LispStruct transformQuoteToLTV(final LispStruct form) {
		// first let's see if we know about this already
		LispStruct newForm = dupSet.get(form);
		// dispatch on type
		if (form instanceof ListStruct) {
			newForm = transformListToLTV((ListStruct) form);
		} else if (form instanceof SymbolStruct) {
			newForm = transformSymbolToLTV((SymbolStruct) form);
		} else if (form instanceof StringStruct) {
			newForm = form;
		} else if (form instanceof RatioStruct) {
//			newForm = transformRatioToLTV((RatioStruct) form);
			newForm = form;
		} else if (form instanceof ComplexStruct) {
//			newForm = transformComplexToLTV((ComplexStruct) form);
			newForm = form;
		} else if (form instanceof IntegerStruct) {
			newForm = form;
		} else if (form instanceof FloatStruct) {
			newForm = form;
		} else if (form instanceof CharacterStruct) {
			newForm = form;
		} else if (form instanceof VectorStruct) {
			newForm = transformSimpleVectorToLTV((VectorStruct) form);
		} else if (form instanceof ArrayStruct) {
			newForm = saSimpleArray((ArrayStruct) form);
		} else {
			throw new RuntimeException("Can't create a load-time-value form for " + form);
		}
		//TODO - must generalize this into becoming make-load-form in the full compiler

		return newForm;
	}

//	private ListStruct transformComplexToLTV(ComplexStruct complex) {
//		// gen (/ numerator denomerator)
//		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.intern("COMPLEX").getSymbolStruct(), complex.getReal(), complex.getImaginary());
//	}

//	private ListStruct transformRatioToLTV(RatioStruct ratio) {
//		// gen (/ numerator denomerator)
//		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.intern("/").getSymbolStruct(), ratio.getBigFraction().getNumerator(), ratio.getBigFraction().getDenominator());
//	}

	private ListStruct transformSymbolToLTV(final SymbolStruct SymbolStruct) {
		if (SymbolStruct.getSymbolPackage() != null) {
			// gen an (intern "sym name" "(find-package "pkg name"))
			final LispStruct[] symbolFindSymbolPatternArray = {GlobalPackageStruct.COMMON_LISP.findSymbol("FIND-PACKAGE").getSymbolStruct(), new StringStruct(SymbolStruct.getSymbolPackage().getName())};
			final LispStruct[] symbolInternPatternArray = {GlobalPackageStruct.COMMON_LISP.findSymbol("INTERN").getSymbolStruct(), new StringStruct(SymbolStruct.getName()), ListStruct.buildProperList(symbolFindSymbolPatternArray)};

			return ListStruct.buildProperList(symbolInternPatternArray);
		} else {
			// has to be a make-symbol unless it's been seen before
			final LispStruct[] symbolMakeSymbolPatternArray = {GlobalPackageStruct.COMMON_LISP.findSymbol("MAKE-SYMBOL").getSymbolStruct(), new StringStruct(SymbolStruct.getName())};
			return ListStruct.buildProperList(symbolMakeSymbolPatternArray);
		}
	}

	private ListStruct transformSimpleVectorToLTV(final VectorStruct formVector) {
		ListStruct formList = NullStruct.INSTANCE;
		for (int i = 0; i < formVector.getTotalSize(); i++) {
			formList = new ConsStruct(formVector.getElementAt(i), formList);
		}
		formList = NReverseFunction.funcall(formList);

		// (system::%make-vector)
		ListStruct functionMakeVector = ListStruct.buildProperList(new SymbolStruct("%MAKE-VECTOR", GlobalPackageStruct.SYSTEM));
		// (function system::%make-vector)
		functionMakeVector = new ConsStruct(SpecialOperatorOld.FUNCTION, functionMakeVector);

		// (((formList)))
		ListStruct finalList = ListStruct.buildProperList(ListStruct.buildProperList(formList));
		// ((quote ((formList))))
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperatorOld.QUOTE, finalList));
		// ((function system::%make-vector) (quote ((formList))))
		finalList = new ConsStruct(functionMakeVector, finalList);
		// ((apply (function system::%make-vector) (quote ((formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct(), finalList));
		return new ConsStruct(SpecialOperatorOld.LOAD_TIME_VALUE, finalList);
	}

	private ListStruct transformListToLTV(ListStruct formList) {
		final List<LispStruct> forms = new Vector<>();
		SymbolStruct listFnSym = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbolStruct();
		while (!formList.equals(NullStruct.INSTANCE)) {
			final LispStruct form = formList.getFirst();
			if (((ConsStruct) formList).getCdr() instanceof ListStruct) {
				forms.add(transformQuoteToLTV(form));
				formList = formList.getRest();
			} else {
				forms.add(transformQuoteToLTV(form));
				// it's a dotted list
				forms.add(transformQuoteToLTV(((ConsStruct) formList).getCdr()));
				// now we have to use LIST*
				listFnSym = GlobalPackageStruct.COMMON_LISP.intern("LIST*").getSymbolStruct();
				break;
			}
		}

		final ListStruct list = ListStruct.buildProperList(forms);
		return new ConsStruct(listFnSym, list);
	}
	//*** end of Quote handling

	private ListStruct saReturnFrom(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();
		final SymbolStruct label = (SymbolStruct) list.getFirst(); // was call to blockSearch

		if (blockStack.search(label) == -1) {
			throw new RuntimeException("return-from: " + list.getFirst() + ", no matching block found");
		}
		list.setElement(1, label);
		list = list.getRest();
		if (!list.getFirst().equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
		}
		return cpy;
	}

	private ListStruct saSetq(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();
		if (list.equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}
		if ((list.size() % 2) == 0) {
			while (!list.equals(NullStruct.INSTANCE)) {
				if (list.getFirst() instanceof SymbolStruct) {
					list.setElement(1, saSymbolStruct((SymbolStruct) list.getFirst()));
					list = list.getRest();
					list.setElement(1, saMainLoop(list.getFirst()));
					list = list.getRest();
				} else {
					throw new RuntimeException("SETQ: " + list.getFirst() + " is not a SymbolStruct.");
				}
			}
		} else {
			throw new RuntimeException("SETQ called with odd number of arguments: " + list);
		}
		return cpy;
	}

	private ListStruct saStaticField(ListStruct list) {
		// 2 args -  the form to eval at class initialization and the name of the field
		list = list.getRest();
		final String fldName = list.getRest().getFirst().toString();
		final LispStruct form = list.getFirst();
		if (form instanceof ListStruct) {
			ListStruct listForm = (ListStruct) form;
			if (listForm.getFirst().equals(SpecialOperatorOld.QUOTE)) {
				((ConsStruct) list).setCdr(saQuote(listForm, fldName));
			} else if (!listForm.getFirst().equals(SpecialOperatorOld.LOAD_TIME_VALUE)) {
				listForm = ListStruct.buildProperList(SpecialOperatorOld.LOAD_TIME_VALUE, form);
				((ConsStruct) list).setCdr(saLoadTimeValue(listForm, fldName));
			} else {
				((ConsStruct) list).setCdr(saLoadTimeValue(listForm, fldName));
			}
		} else {
			final ListStruct listForm = ListStruct.buildProperList(SpecialOperatorOld.LOAD_TIME_VALUE, form);
			((ConsStruct) list).setCdr(saLoadTimeValue(listForm, fldName));
		}
		return NullStruct.INSTANCE; // we've done what we need to do, the form needs to be dropped
	}

	private ListStruct saSymbolMacrolet(final ListStruct list) {
		LOGGER.error("; Warning: MACROLET not supported at this time.");
		return list;
	}

	private LispStruct saTagbody(ListStruct list) {
		ListStruct listCopy = NullStruct.INSTANCE;

		// Read all the tags within the TAGBODY.
		final Stack tagStack = tagbodyReadLabels(list.getRest());
		// it's possible that there are no tags in the tagbody.
		// If so, we just turn it into a PROGN and go on our merry way...EXCEPT
		// tagbody is defined to return NIL
		if (tagStack.empty()) {
			// change the TAGBODY to PROGN
			list.setElement(1, SpecialOperatorOld.PROGN);
			// then append a NIL to the end
			list = (ListStruct) AppendFunction.funcall(list, ListStruct.buildProperList(NullStruct.INSTANCE));
			return saMainLoop(list);
		}

		list = list.getRest();
		tagbodyStack.push(tagStack);

        /* Walk through the list and invoke the semantic analyzer on each
         * non-tag element. */
		while (!list.equals(NullStruct.INSTANCE)) {
			final LispStruct obj = list.getFirst();
			if (obj instanceof SymbolStruct) {
				listCopy = new ConsStruct(obj, listCopy);
			} else {
				listCopy = new ConsStruct(saMainLoop(obj), listCopy);
			}
			list = list.getRest();
		}
		// Pop the tag stack for this TAGBODY from the global stack.
		tagbodyStack.pop();
		final ListStruct result = NReverseFunction.funcall(listCopy);
		return new ConsStruct(SpecialOperatorOld.TAGBODY, result);
	}

	/* Reads all the tags in the TAGBODY form and inserts them into a stack
	 * which is returned. Its necessary to do this first since a GO can be
	 * executed for a tag declared later in the form. */
	private Stack tagbodyReadLabels(ListStruct list) {
		final Stack tagStack = new Stack();

		while (!list.equals(NullStruct.INSTANCE)) {
			final LispStruct obj = list.getFirst();
			if ((obj instanceof SymbolStruct)
					|| (obj instanceof Number)) {
				// Insert the tag and its new SymbolStruct into the stack.
				final SymbolStruct newSym = new SymbolStruct("Tagbody" + iTagbodyCounter);
				iTagbodyCounter++;
				tagStack.push(new ConsStruct(obj, newSym));
				list.setElement(1, newSym);
			}
			list = list.getRest();
		}
		return tagStack;
	}

	/*
	 * Ignores the type specialization
	 */
	private LispStruct saThe(final ListStruct list) {
		return list.getRest().getRest().getFirst();
	}

	private ListStruct saThrow(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();

		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		return cpy;
	}

	private ListStruct saUnwindProtect(ListStruct list) {
		final ListStruct cpy = list;
		list = list.getRest();

		while (!list.equals(NullStruct.INSTANCE)) {
			list.setElement(1, saMainLoop(list.getFirst()));
			list = list.getRest();
		}
		return cpy;
	}

	private ListStruct saMacroLambda(final ListStruct list) {
		// if there is the new macro parser, use it
		// if not, do the original bootstrap
		if (false) {
			// call the macro parser and set up the updated list
			return (ListStruct) saMainLoop(list);
		} else {
			return saLambda(list);
		}
	}

	// New helper function. It takes a parsedLambdaList and removes a FakeRest used for setting up the lambda
	// list for function application
	private ListStruct removeFakeRestEntry(final ListStruct parsedLambdaList) {
		// Only do nothing unless the parsedLambdaList has a FakeRest
		ListStruct thePLL = parsedLambdaList;
		final SymbolStruct fakeRestSymbolStruct = GlobalPackageStruct.COMPILER.findSymbol(
				"FakeRestSymbolStructForHandlingKeysWithout&Rest").getSymbolStruct();
		while (!thePLL.equals(NullStruct.INSTANCE)) {
			// if we find a fakeRestSymbolStruct at the beginning of a lambda list, remove it
			if (((ListStruct) thePLL.getFirst()).getFirst().equals(fakeRestSymbolStruct)) {
				// found one!
				return removeFakeRestEntryAux(parsedLambdaList);
			} else {
				thePLL = thePLL.getRest();
			}
		}
		return parsedLambdaList;
	}

	private ListStruct removeFakeRestEntryAux(final ListStruct parsedLambdaList) {
		// doesn't use the Collection framework remove because the check has to be inside the cons
		// So, we do it the hard way
		final List<LispStruct> copyJavaList = parsedLambdaList.getAsJavaList();
		final boolean isPLLDotted = parsedLambdaList.isDotted();

		final ListStruct copyParsedLambdaList;
		if (isPLLDotted) {
			copyParsedLambdaList = ListStruct.buildDottedList(copyJavaList);
		} else {
			copyParsedLambdaList = ListStruct.buildProperList(copyJavaList);
		}

		final SymbolStruct fakeRestSymbolStruct = GlobalPackageStruct.COMPILER.findSymbol(
				"FakeRestSymbolStructForHandlingKeysWithout&Rest").getSymbolStruct();
		// Check to see if the first element is the fake. This happens in the case of
		// of the param list starts with &key
		final ListStruct parsedLambdaElt = (ListStruct) copyParsedLambdaList.getFirst();
		if (parsedLambdaElt.getFirst().equals(fakeRestSymbolStruct)) {
			// just return the rest of the parsed lambda elements
			return adjustParameterNbrs(copyParsedLambdaList.getRest()); // NOTE: decr the parameter
		}
		// here the fake rest is embedded
		ListStruct prior = copyParsedLambdaList;
		ListStruct current = copyParsedLambdaList.getRest();
		// see if we have any
		while (!current.equals(NullStruct.INSTANCE)) {
			if (((ListStruct) current.getFirst()).getFirst().equals(fakeRestSymbolStruct)) {
				// now splice it out
				// now we spice out the fake rest entry
				((ConsStruct) prior).setCdr(current.getRest());
				// now adjust the parameters that are post- fake element
				adjustParameterNbrs(current.getRest());
				// now drop the first element to the garbage
				((ConsStruct) current).setCdr(NullStruct.INSTANCE);
				break;
				// now we have to decr the parameter parameter (really) to account for no param slot for the fake
			}
			prior = prior.getRest();
			current = current.getRest();
		}
		return parsedLambdaList;
	}

	private ListStruct adjustParameterNbrs(final ListStruct paramsToAdjust) {
		final SymbolStruct allocKey = GlobalPackageStruct.KEYWORD.findSymbol("ALLOCATION").getSymbolStruct();
		// Now we have to rework the parameter indices since we just dropped a paramter (the fake rest)
		ListStruct adjustableList = paramsToAdjust;
		while (!adjustableList.equals(NullStruct.INSTANCE)) {
			// for each &key params, get the parameter number, decr it, and put it back
			final ListStruct params = ((ListStruct) adjustableList.getFirst()).getRest(); // now we have a plist
			// get the parameter cons entry and decr the cdr number
			// now let's print it
			final ConsStruct theParamAlloc = (ConsStruct) GetPlist.funcall(params,
					allocKey);
			// now adjust the number found in the cdr
			final int paramNbr = ((IntegerStruct) theParamAlloc.getCdr()).getBigInteger().intValue();
			// decr the number by 1
			theParamAlloc.setCdr(new IntegerStruct(BigInteger.valueOf(paramNbr - 1)));
			adjustableList = adjustableList.getRest();
		}
		return paramsToAdjust;
	}

	private ListStruct saLambda(final ListStruct list) {
		// macroexpand the LAMBDA form, by default makes it (FUNCTION (LAMBDA... ))
		final Object[] newList = (Object[]) MacroExpandFunction.FUNCTION.funcall(list);
		// now it has the right form, now handle it to saFunction
		if (newList[1].equals(NullStruct.INSTANCE)) {
			final ListStruct inter = ListStruct.buildProperList((LispStruct) newList[0]);
			newList[0] = new ConsStruct(SpecialOperatorOld.FUNCTION, inter);
		}
		return saFunction((ListStruct) newList[0]);
	}

	private ListStruct saLambdaAux(ListStruct list) {
		if (list.size() < 2) {
			throw new RuntimeException("Incorrectly formed lambda expression, size: " + list.size());
		}

		//might also need here prev. bindings position num, or even a stack?
		final int tempPosition = bindingsPosition;
		final Environment parentEnvironment = environmentStack.peek();

		// keep track of the current environment as it is "now"
		final Environment newEnvironment;
		// make a new environment and set it to "current"
		final SymbolStruct operator = (SymbolStruct) list.getFirst();
		if (operator.equals(SpecialOperatorOld.MACRO_LAMBDA)) {
			newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACRO);
		} else {
			newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LAMBDA);
		}
		// set the current environment's parent to what was the environment
		environmentStack.push(EnvironmentAccessor.createParent(newEnvironment, parentEnvironment));
		try {
			dupSetStack.push(dupSet);
			dupSet = new IdentityHashMap<>();

			// list => (%lambda (lambda-list) (declare ...) ?"...doc..." body...)
			final SymbolStruct lambdaSym = (SymbolStruct) list.getFirst(); // hang on to %lambda or %macro

			list = list.getRest(); // step over lambda
			// list -> ((lambda-list) ?(declare ...) ?"...doc..." body...)
			// get the parameter list
			ListStruct params = (ListStruct) list.getFirst();
			// params -> (lambda-list)
			list = list.getRest(); // step over the parameter list
			// list -> (?(declare ...)  ?"...doc..."body...)
			// handle any declarations and doc string that might be present
			// returns an updated list with at least one DECLARATION
			// doc string, if present, is now in a declaration
			list = saDeclarations(list);
			// list -> ((declare ...) body...)
			final ListStruct decls = (ListStruct) list.getFirst();
			// decls -> (declare ...)

			// Now, see if there is already a parsed lambda list
			// this would happen when the lambda was generated from a defun
			ListStruct theParsedLambdaList = NullStruct.INSTANCE;
			ListStruct auxValues = null;
			if (!params.equals(NullStruct.INSTANCE)) {
				if (theParsedLambdaList.equals(NullStruct.INSTANCE)) {
					final PackageSymbolStruct pOLL = GlobalPackageStruct.COMPILER.findSymbol("PARSE-ORDINARY-LAMBDA-LIST");
					FunctionStruct parseOrdinaryLambdaListFn = null;
					if (pOLL.getPackageSymbolType() != null) {
						parseOrdinaryLambdaListFn = pOLL.getSymbolStruct().getFunction();
					}
					if ((parseOrdinaryLambdaListFn != null) && // the Lisp code is loaded
							lambdaSym.equals(SpecialOperatorOld.LAMBDA)) { // it's a lambda not a macro
						final ListStruct parsedLambdaListParts = (ListStruct) parseOrdinaryLambdaListFn.apply(params);
						theParsedLambdaList = (ListStruct) parsedLambdaListParts.getFirst();
						auxValues = (ListStruct) parsedLambdaListParts.getElement(1);
						// Now, use the Compiler function provide-init-form-usage to create code
						// that follows the declaration section. It is a (possibly nil) set of conditional
						// assignments that fill in the default forms for missing arguments

						// Add the lambda bindings to the current environment
						final ConsStruct bindingList = (ConsStruct) EnvironmentAccessor.getBindingSet(newEnvironment);
						// Now I have to remove any FAKE rest forms. They can't be in the binding set
						bindingList.setCdr(removeFakeRestEntry(theParsedLambdaList));
					}
				}
			}
			// temporary hack to handle tail-called functions here
			currentParsedLambdaList.push(theParsedLambdaList);

			// classname is a declaration in the declare form
			// if there isn't one, it makes one up
			final SymbolStruct classname = saGetClassName(decls);

			// now reconstitute the full lambda form
			list = new ConsStruct(params, list);
			list = new ConsStruct(lambdaSym, list);
			// list => (%lambda (lambda-list) (declare ...) body...)
			final ListStruct cpy = list;

			// step over the lambda SymbolStruct to work on the params etc
			list = list.getRest();

			//TODO This section will be replaced with the Lisp parser
			// the current min for binding is 1 since it's a lambda
			bindingsPosition = 0;
			// handled setting up the bindings differently between using the old way and the
			if (!params.equals(NullStruct.INSTANCE)) {
				if (!theParsedLambdaList.equals(NullStruct.INSTANCE) && lambdaSym.equals(SpecialOperatorOld.LAMBDA)) {
					// NOW - the first thing to do is make the arglist analyzer function for this lambda
					// NOTE: this is all skipped when the lambda has a declaration of %NO-GENERATE-ANALYZER
					final PackageSymbolStruct genAnalyzerSym = GlobalPackageStruct.COMPILER.findSymbol("GENERATE-ARGLIST-ANALYZER");
					final FunctionStruct genAnalyzerFn = genAnalyzerSym.getSymbolStruct().getFunction();
					// don't an analyzer for the analyzer!
					if ((genAnalyzerFn != null)
							&& findDeclaration(DeclarationOld.NO_GENERATE_ANALYZER, decls.getRest()).equals(NullStruct.INSTANCE)) {
						final ListStruct analyzerFn = (ListStruct) genAnalyzerFn.apply(theParsedLambdaList);
						//*** Under some certain circumstances, the function has to be compiled for use in the rest
						//*** of the function definition. The most common reason is the function is recursive
						//*** and the arglist must be munged before the function is compiled
						// If the function that's being compiled is explicitly named,
						// then we compile the munger and put it into an association list keyed by function name
						if (currentLispName.peek() != null) {
							final ListStruct copyFn = (ListStruct) XCopyTreeFunction.FUNCTION.funcall(analyzerFn);
							final FunctionStruct innerFn = (FunctionStruct) CompileFunction.FUNCTION.funcall(copyFn);
							final FunctionStruct munger = (FunctionStruct) innerFn.apply();
							currentArgMunger = new ConsStruct(new ConsStruct(currentLispName.peek(), munger), currentArgMunger);
						}

						// This has created an unevaluated lambda form. We now
						// have to stick it into a static field in the current lambda
						saStaticField(ListStruct.buildProperList(NullStruct.INSTANCE, analyzerFn, LAMBDA_ARGLIST_MUNGER_STRING));
						// this static field holds lambdas that provide init values. The lambdas
						// are evaluated in the right environment. The lambdas are values in a property
						// list that is keyed by the name of the parameter.
					}
					final ListStruct declsOnward = list.getRest();
					// ((declare ...) body...)
					ListStruct afterDecls = declsOnward.getRest();
					// ((body...))

					// now I have to splice in any init form code before the (block foo...
					ListStruct blockCdr = NullStruct.INSTANCE;
					if ((afterDecls.getFirst() instanceof ListStruct)
							&& ((ListStruct) afterDecls.getFirst()).getFirst().equals(SpecialOperatorOld.BLOCK)) {
						// this section used when fn was created by DEFUN
						blockCdr = ((ListStruct) afterDecls.getFirst()).getRest().getRest();
					} else {
						// push NIL
						afterDecls = new ConsStruct(NullStruct.INSTANCE, afterDecls);
//                        // push BLOCK
						afterDecls = new ConsStruct(SpecialOperatorOld.BLOCK, afterDecls);
						afterDecls = ListStruct.buildProperList(afterDecls);
						blockCdr = ((ListStruct) afterDecls.getFirst()).getRest().getRest();
						;
					}

					final PackageSymbolStruct provideInitFormUsage = GlobalPackageStruct.COMPILER.findSymbol("PROVIDE-INIT-FORM-USAGE");
					final FunctionStruct provideInitFormUsageFn = provideInitFormUsage.getSymbolStruct().getFunction();

					if (provideInitFormUsageFn != null) {
						final ListStruct insert = (ListStruct) provideInitFormUsageFn.apply(theParsedLambdaList);
						// splice the code into the afterDecl
						if (!insert.equals(NullStruct.INSTANCE)) {
							afterDecls = (ListStruct) AppendFunction.funcall(insert, afterDecls);
//                            ((ConsStruct)funLast.funcall(insert)).setCdr(blockCdr);
//                            ((ConsStruct)((ListStruct)afterDecls.getFirst()).getRest()).setCdr(insert);
						}
						// let's see what's going with
					}
					// there may be &aux VariableOlds that get turned into a let* that starts before the
					// block construct.
					if ((auxValues != null) && !auxValues.equals(NullStruct.INSTANCE)) {
						afterDecls = new ConsStruct(SpecialOperatorOld.LET_STAR, new ConsStruct(auxValues, afterDecls));
						afterDecls = new ConsStruct(afterDecls, NullStruct.INSTANCE);
					}

					// now splice the static-field for as the first line of the body
					((ConsStruct) declsOnward).setCdr(afterDecls);
				} else {
					while (!params.equals(NullStruct.INSTANCE)) {
						addBinding_LambdaFletLabels((SymbolStruct) params.getFirst(), ++bindingsPosition);
						params.setElement(1, ((ListStruct) bindings.getFirst()).getFirst());
						params = params.getRest();
					}
				}
			}

			//************** Now we work on the body of the LAMBDA ***********
			// if there's an empty body, make it just a NIL
			if (list.getRest().equals(NullStruct.INSTANCE)) {
				((ConsStruct) list).setCdr(new ConsStruct(NullStruct.INSTANCE, NullStruct.INSTANCE));
			}

			// list
			list = list.getRest();
			final ListStruct listCopy = list;
			while (!list.equals(NullStruct.INSTANCE)) {
				list.setElement(1, saMainLoop(list.getFirst()));
				list = list.getRest();
			}

			//set the current environment back to what it was before we hit this method
			return new ConsStruct(environmentStack.peek(), listCopy);
		} finally {
			bindingsPosition = tempPosition;  //???
			environmentStack.pop();
			// done with the current hack for tail-called functions
			currentParsedLambdaList.pop();
			currentLispName.pop();
			dupSet = dupSetStack.pop();
		}
	}

	private SymbolStruct saGetClassName(ListStruct list) {
		// check to see if any declarations exist
		if (list.getFirst().equals(SpecialOperatorOld.DECLARE)) {
			// strip out DECLARATION keyword
			list = list.getRest();
			return (SymbolStruct) getCadr(AssocFunction.funcall(DeclarationOld.JAVA_CLASS_NAME, list));
		}
		return null;
	}

	private void addBinding_FBinding(final SymbolStruct sym, final Object initForm, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewFBinding(environmentStack.peek(), sym, position,
				(SymbolStruct) GensymFunction.funcall(javafy(sym) + System.currentTimeMillis()), isSpecial);
	}

	private void addBinding_Let(final SymbolStruct sym, final int position, final LispStruct initForm) {
		addBinding_Let(sym, position, initForm, false);
	}

	private void addBinding_Let(final SymbolStruct sym, final int position, final LispStruct initForm, final boolean isSpecial) {
		EnvironmentAccessor.createNewLetBinding(environmentStack.peek(), sym, position, initForm, isSpecial);
	}

	private void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position) {
		addBinding_LambdaFletLabels(sym, position, false);
	}

	private void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewLambdaBinding(environmentStack.peek(), sym, position, isSpecial);
	}

	// a utility to replace the use of Cadr
	private LispStruct getCadr(final ListStruct lst) {
		return lst.getRest().getFirst();
	}
}
