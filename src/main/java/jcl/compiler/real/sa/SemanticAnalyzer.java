package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.StringStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.WrapInLambda;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.old.functions.NReverseFunction;
import jcl.compiler.old.functions.XCopyTreeFunction;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.compiler.real.sa.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.specialoperator.QuoteAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.Declaration;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Stack;
import java.util.Vector;
import java.util.regex.Pattern;

public class SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzer.class);

	public static final SymbolStruct Defmacro = GlobalPackageStruct.COMMON_LISP.findSymbol("DEFMACRO").getSymbolStruct();
	public static final SymbolStruct COMPILE_TOPLEVEL = KeywordOld.CompileToplevel;
	public static final SymbolStruct LOAD_TOPLEVEL = KeywordOld.LoadToplevel;
	private static final StringStruct LAMBDA_ARGLIST_MUNGER_STRING = new StringStruct("LAMBDA_ARGLIST_MUNGER");

	// eval-when processing modes
	private boolean topLevelMode;

	static String nameBreakingRegex = "[^\\p{Alnum}]";
	static Pattern nameBreakingPattern = Pattern.compile(nameBreakingRegex);
	private static ListStruct bindings;
	public static Stack<Environment> environmentStack;
	public static Stack<SymbolStruct> blockStack;
	public static Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack;
	public static int iTagbodyCounter;
	private static Vector<SymbolStruct> undefinedFunctions;
	private static Stack<ListStruct> currentParsedLambdaList;
	private static Stack<SymbolStruct> currentLispName;
	private static Stack<IdentityHashMap<LispStruct, LispStruct>> dupSetStack;
	public static IdentityHashMap<LispStruct, LispStruct> dupSet = null;
	public static int bindingsPosition;
	// and association of function names seen and their arglist munging
	// used to handle recursive functions
	private static ListStruct currentArgMunger = NullStruct.INSTANCE;

	private void initialize() {
		//create the global environment
		environmentStack = new Stack<>();
		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());
		currentParsedLambdaList = new Stack<>();
		currentParsedLambdaList.push(NullStruct.INSTANCE);
		currentLispName = new Stack<>();
		currentLispName.push(null);

		topLevelMode = true;

		blockStack = new Stack<>();
		tagbodyStack = new Stack<>();
		iTagbodyCounter = 0;
		undefinedFunctions = new Vector<>();
		bindingsPosition = 0;
		bindings = NullStruct.INSTANCE;
		dupSetStack = new Stack<>();
	}

	public LispStruct funcall(LispStruct form) {
		initialize();
		if (!(form instanceof ListStruct)
				|| !(((ListStruct) form).getFirst() instanceof SymbolStruct)
				|| !(((ListStruct) form).getFirst().equals(SpecialOperator.LAMBDA)
				|| ((ListStruct) form).getFirst().equals(SpecialOperator.MACRO_LAMBDA))) {
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

	public static LispStruct saMainLoop(LispStruct form) {
		if (form.equals(NullStruct.INSTANCE)) {
			// do nothing
		} else if (form instanceof KeywordSymbolStruct) {
			//do nothing
		} else if (form instanceof CharacterStruct) {
			//do nothing
		} else if (form instanceof NumberStruct) {
			//do nothing
		} else if (form instanceof SymbolStruct) {
			form = SymbolStructAnalyzer.INSTANCE.analyze((SymbolStruct<?>) form);
		} else if (form instanceof ListStruct) {
			form = ListStructAnalyzer.INSTANCE.analyze((ListStruct) form);
		} else if (form instanceof VectorStruct) {
			form = saVectorImpl((VectorStruct) form);
		} else if (form instanceof ArrayStruct) {
			form = saSimpleArray((ArrayStruct) form);
		} else {
			LOGGER.warn("SA: Found thing I can't semantically analyze: {}, class: {}", form, form.getClass().getName());
		}
		return form;
	}

	/*
	 *********************************************************
	 * Analyzers
	 *********************************************************
	 */

	private static LispStruct saVectorImpl(final VectorStruct formVector) {

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
		functionMakeArray = new ConsStruct(SpecialOperator.FUNCTION, functionMakeArray);

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
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperator.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("APPLY").getSymbolStruct(), finalList));
		final ListStruct ltvList = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, finalList);
		return saMainLoop(ltvList);
	}

	// ********** START OF saSimpleArray() TRANSLATOR ********** //
	public static ListStruct saSimpleArray(final ArrayStruct formArray) {
		final ListStruct formList = createSimpleArrayInitialContentsList(formArray);

		// (system::%make-array)
		ListStruct functionMakeArray = ListStruct.buildProperList(new SymbolStruct("%MAKE-ARRAY", GlobalPackageStruct.SYSTEM));
		// (function system::%make-array)
		functionMakeArray = new ConsStruct(SpecialOperator.FUNCTION, functionMakeArray);

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
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperator.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("APPLY").getSymbolStruct(), finalList));
		final ListStruct ltvList = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, finalList);
		return (ListStruct) saMainLoop(ltvList);
	}

	private static ListStruct createSimpleArrayInitialContentsList(final ArrayStruct formArray) {
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

	private static ListStruct initialContentsBuilder(ListStruct formList, final ListStruct dimensionsList) {
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

	private static ListStruct subListFinder(ListStruct formList, final ListStruct dimensionsList) {
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

	private static ListStruct reduceListSize(ListStruct formList, final ListStruct dimensionsList) {

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

	/*
	 *********************************************************
	 * Special Operator Analyzers
	 *********************************************************
	 */

	public static Map<SymbolStruct<?>, SymbolStruct<?>> getFunctionNames(final String functionBinding, final ListStruct listStruct,
																		 final List<LispStruct> functionJavaList) {
		final Map<SymbolStruct<?>, SymbolStruct<?>> functionNameMap = new HashMap<>();

		for (final LispStruct currentFunction : functionJavaList) {
			if (!(currentFunction instanceof ListStruct)) {
				throw new RuntimeException("Improperly formed " + functionBinding + ": " + listStruct);
			}

			final ListStruct functionListStruct = (ListStruct) currentFunction;
			final LispStruct functionFirst = functionListStruct.getFirst();
			if (!(functionFirst instanceof SymbolStruct)) {
				throw new RuntimeException("Improperly formed " + functionBinding + ": " + listStruct);
			}

			final SymbolStruct<?> functionName = (SymbolStruct) functionFirst;
			final SymbolStruct<?> gensymFunctionName = GensymFunction.funcall(functionName.getName());
			functionNameMap.put(functionName, gensymFunctionName);
		}

		return functionNameMap;
	}

	/*
	 *********************************************************
	 * OTHER STUFF
	 *********************************************************
	 */


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
					if (test.equals(SpecialOperator.LAMBDA_MARKER) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.MACRO_MARKER)) {
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
					if (test.equals(SpecialOperator.LAMBDA_MARKER) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.LABELS) || test.equals(SpecialOperator.FLET) || test.equals(SpecialOperator.MACRO_MARKER)) {
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
					if (!theCar.equals(SpecialOperator.DECLARE)) {
						saSetClosureDepth(workingList.getRest(), depth);
					}
				}
			}
		}
		return form;
	}

	public static FunctionStruct LAMBDA_ARGLIST_MUNGER =
			new FunctionStruct() {

				public LispStruct funcall(final LispStruct arglist, final LispStruct marker) {
					return new ConsStruct(SpecialOperator.FUNCTION_MARKER,
							new ConsStruct(marker, arglist));
				}

				@Override
				public LispStruct apply(final LispStruct... lispStructs) {
					return funcall(lispStructs[0], lispStructs[1]);
				}
			};

	public static ListStruct saFunctionCall(ListStruct list) {
		ListStruct cpy = list;
		// check to see if there's a function binding in existence
		if (list.getFirst() instanceof SymbolStruct) {
			SymbolStruct fnName = (SymbolStruct) list.getFirst();
			//TODO make this active only during compiling, not during eval during compile-file
			// handle messaging the argument list according to the lambda list
			// if the car of the function is a special marker, drop the marker
			if (fnName.equals(SpecialOperator.FUNCTION_MARKER)) {
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
			cpy = new ConsStruct(SpecialOperator.TAIL_RECURSION, cpy);
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
	private static ListStruct saDeclarations(ListStruct list) {
		// list is the rest of the forms after the arg list
		ListStruct newDecls = NullStruct.INSTANCE;   //the list of all new Declarations
		final ListStruct returnList = list;

		while (!list.equals(NullStruct.INSTANCE)) {
			LispStruct theCar = list.getFirst();
			ListStruct theDecl;    //the current new Declaration
			if (theCar instanceof CharSequence) {
				// This may be a doc string, unless it's the last thing
				if (list.getRest().equals(NullStruct.INSTANCE)) {
					break;
				} else {
					// there's stuff after the doc string so it is doc
					theDecl = ListStruct.buildProperList(ListStruct.buildProperList(Declaration.DOCUMENTATION, theCar));
					newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
					list = list.getRest();

					// add code to look for more declarations - without any strings
					theCar = list.getFirst();
					while ((theCar instanceof ListStruct)
							&& ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
						theDecl = ((ListStruct) theCar).getRest();
						newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
						list = list.getRest();
						theCar = list.getFirst();
					}
					// saDeclarationsHelp will complete the search for declarations
					break;
				}
			}
			if ((theCar instanceof ListStruct) && ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
				theDecl = ((ListStruct) theCar).getRest();
				newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
			} else {
				break;
			}
			list = list.getRest();
		}

		// if there isn't a Declaration.JAVA_CLASS_NAME, add one
		final ListStruct classNameDecl = AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, newDecls);
		if (classNameDecl.equals(NullStruct.INSTANCE)) {
			final SymbolStruct classname;
			// if there's a Lisp Name, then Javafy it
			final ListStruct lispNameDecl = AssocFunction.funcall(Declaration.LISP_NAME, newDecls);
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
			newDecls = new ConsStruct(new ConsStruct(Declaration.JAVA_CLASS_NAME, ListStruct.buildProperList(classname)), newDecls);
		}
		currentLispName.push(null);
		// if the value is a String, make it into a SymbolStruct
		final Object name = classNameDecl.getRest().getFirst();
		if (name instanceof CharSequence) {
			final ListStruct rest = classNameDecl.getRest();
			rest.setElement(1, new SymbolStruct(name.toString()));
		}
		// now we conjure a new declaration
		return new ConsStruct(new ConsStruct(SpecialOperator.DECLARE, newDecls), list);
	}

	// routine to javafy a Lisp SymbolStruct or string to a Java identifier
	private static String javafy(final Object lispName) {
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

	private static ListStruct findDeclaration(final SymbolStruct item, final ListStruct list) {
		return AssocFunction.funcall(item, list);
	}

	public static ListStruct saDeclare(final ListStruct list) {
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
			if (declIdentifier.equals(Declaration.LISP_NAME)) {
				saLispNameDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.JAVA_CLASS_NAME)) {
				saJavaNameDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.NO_GENERATE_ANALYZER)) {
				saNoGenerateAnalyzerDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.PARSED_LAMBDA_LIST)) {
				saParsedLambdaListDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.DOCUMENTATION)) {
				saDocumentationDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.SOURCE_FILE)) {
				saSourceFileDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.DECLARATION)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DYNAMIC_EXTENT)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.FTYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORABLE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.INLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.NOTINLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.OPTIMIZE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.SPECIAL)) {
				saSpecialDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
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
	private static void saLispNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saJavaNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saNoGenerateAnalyzerDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the parsed lambda list of the function
	 */
	private static void saParsedLambdaListDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saDocumentationDeclaration(final ListStruct declarationList) {
	}

	private static void saSourceFileDeclaration(final ListStruct declarationList) {
	}

	/**
	 * handle the components of a Special declaration
	 */
	private static void saSpecialDeclaration(final ListStruct declarations) {
		final List<LispStruct> declaractionsJavaList = declarations.getAsJavaList();
		SymbolStruct sym = null;
		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct nextDecl : declaractionsJavaList) {
			try {
				sym = (SymbolStruct) nextDecl;
				SymbolStructAnalyzer.INSTANCE.analyze(sym);
			} catch (final ClassCastException ccExcption) {
				LOGGER.error("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: {}", sym);
			}
		}
	}

	public static ListStruct saDefstruct(final ListStruct list) {
		return list;
	}


	public static ListStruct saStaticField(ListStruct list) {
		// 2 args -  the form to eval at class initialization and the name of the field
		list = list.getRest();
		final String fldName = list.getRest().getFirst().toString();
		final LispStruct form = list.getFirst();
		if (form instanceof ListStruct) {
			ListStruct listForm = (ListStruct) form;
			if (listForm.getFirst().equals(SpecialOperator.QUOTE)) {
				((ConsStruct) list).setCdr(QuoteAnalyzer.INSTANCE.analyze(listForm, fldName));
			} else if (!listForm.getFirst().equals(SpecialOperator.LOAD_TIME_VALUE)) {
				listForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, form);
				((ConsStruct) list).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
			} else {
				((ConsStruct) list).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
			}
		} else {
			final ListStruct listForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, form);
			((ConsStruct) list).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
		}
		return NullStruct.INSTANCE; // we've done what we need to do, the form needs to be dropped
	}

	public static ListStruct saMacroLambda(final ListStruct list) {
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
	private static ListStruct removeFakeRestEntry(final ListStruct parsedLambdaList) {
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

	private static ListStruct removeFakeRestEntryAux(final ListStruct parsedLambdaList) {
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

	private static ListStruct adjustParameterNbrs(final ListStruct paramsToAdjust) {
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

	public static ListStruct saLambda(final ListStruct list) {
		// macroexpand the LAMBDA form, by default makes it (FUNCTION (LAMBDA... ))
		final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(list);
		// now it has the right form, now handle it to saFunction

		ListStruct functionToAnalyze = (ListStruct) macroExpandReturn.getExpandedForm();
		if (!macroExpandReturn.wasExpanded()) {
			final ListStruct inter = ListStruct.buildProperList(functionToAnalyze);
			functionToAnalyze = new ConsStruct(SpecialOperator.FUNCTION, inter);
		}
		return (ListStruct) FunctionAnalyzer.INSTANCE.analyze(functionToAnalyze);
	}

	public static ListStruct saLambdaAux(ListStruct list) {
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
		if (operator.equals(SpecialOperator.MACRO_LAMBDA)) {
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
							lambdaSym.equals(SpecialOperator.LAMBDA)) { // it's a lambda not a macro
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
				if (!theParsedLambdaList.equals(NullStruct.INSTANCE) && lambdaSym.equals(SpecialOperator.LAMBDA)) {
					// NOW - the first thing to do is make the arglist analyzer function for this lambda
					// NOTE: this is all skipped when the lambda has a declaration of %NO-GENERATE-ANALYZER
					final PackageSymbolStruct genAnalyzerSym = GlobalPackageStruct.COMPILER.findSymbol("GENERATE-ARGLIST-ANALYZER");
					final FunctionStruct genAnalyzerFn = genAnalyzerSym.getSymbolStruct().getFunction();
					// don't an analyzer for the analyzer!
					if ((genAnalyzerFn != null)
							&& findDeclaration(Declaration.NO_GENERATE_ANALYZER, decls.getRest()).equals(NullStruct.INSTANCE)) {
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
							&& ((ListStruct) afterDecls.getFirst()).getFirst().equals(SpecialOperator.BLOCK)) {
						// this section used when fn was created by DEFUN
						blockCdr = ((ListStruct) afterDecls.getFirst()).getRest().getRest();
					} else {
						// push NIL
						afterDecls = new ConsStruct(NullStruct.INSTANCE, afterDecls);
//                        // push BLOCK
						afterDecls = new ConsStruct(SpecialOperator.BLOCK, afterDecls);
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
						afterDecls = new ConsStruct(SpecialOperator.LET_STAR, new ConsStruct(auxValues, afterDecls));
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

	private static SymbolStruct saGetClassName(ListStruct list) {
		// check to see if any declarations exist
		if (list.getFirst().equals(SpecialOperator.DECLARE)) {
			// strip out DECLARATION keyword
			list = list.getRest();
			return (SymbolStruct) AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, list).getRest().getFirst();
		}
		return null;
	}

	private void addBinding_FBinding(final SymbolStruct sym, final Object initForm, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewFBinding(environmentStack.peek(), sym, position,
				(SymbolStruct) GensymFunction.funcall(javafy(sym) + System.currentTimeMillis()), isSpecial);
	}

	private static void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position) {
		addBinding_LambdaFletLabels(sym, position, false);
	}

	private static void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewLambdaBinding(environmentStack.peek(), sym, position, isSpecial);
	}
}
