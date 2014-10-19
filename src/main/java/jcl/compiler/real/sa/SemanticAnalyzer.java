package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.specialoperator.BlockAnalyzer;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.compiler.real.sa.specialoperator.TagbodyAnalyzer;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.NILStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

public class SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzer.class);

	// eval-when processing modes
	private boolean topLevelMode;

	public static Stack<Environment> environmentStack;
	public static List<SymbolStruct<?>> undefinedFunctions;
	public static Stack<SymbolStruct<?>> currentLispName;
	public static int bindingsPosition;
	// and association of function names seen and their arglist munging
	// used to handle recursive functions
	public static ListStruct currentArgMunger = NullStruct.INSTANCE;

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

	private void initialize() {
		//create the global environment
		environmentStack = new Stack<>();
		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());
		currentLispName = new Stack<>();
		currentLispName.push(null);

		topLevelMode = true;

		BlockAnalyzer.BLOCK_STACK.clear();
		TagbodyAnalyzer.TAGBODY_STACK.clear();
		undefinedFunctions = Collections.synchronizedList(new ArrayList<>());
		bindingsPosition = 0;
		FunctionAnalyzer.bindings = NullStruct.INSTANCE;
	}

	public LispStruct funcall(LispStruct form) {
		initialize();

		if (!(form instanceof ListStruct)) {
			form = ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("LAMBDA").getSymbolStruct(), NullStruct.INSTANCE, form);
		} else {
			final ListStruct formList = (ListStruct) form;
			final LispStruct firstOfFormList = formList.getFirst();
			if (!(firstOfFormList instanceof SymbolStruct)
					|| !(firstOfFormList.equals(SpecialOperator.LAMBDA) || firstOfFormList.equals(SpecialOperator.MACRO_LAMBDA))) {
				form = ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("LAMBDA").getSymbolStruct(), NullStruct.INSTANCE, form);
			}
		}


		// make a copy so we can trash it in SA
		form = saMainLoop(form);
		// now setup the closure depths
		form = saSetClosureDepth(form, 0);
		// clear the dup hash map

		// now see if we have any functions still undefined
		for (final SymbolStruct<?> undefinedFunction : undefinedFunctions) {
			if (undefinedFunction.getFunction() == null) {
				LOGGER.warn("; Warning: no function or macro function defined for ");
				if (undefinedFunction.getSymbolPackage() != null) {
					LOGGER.warn("{}::{}", undefinedFunction.getSymbolPackage().getName(), undefinedFunction.getName());
				} else {
					LOGGER.warn("#:{}", undefinedFunction.getName());
				}
			}
		}

		return form;
	}

	public static LispStruct saMainLoop(final LispStruct form) {

		LispStruct analyzedForm = form;
		if ((form instanceof SymbolStruct) && !form.equals(NILStruct.INSTANCE)) {
			analyzedForm = SymbolStructAnalyzer.INSTANCE.analyze((SymbolStruct<?>) form);
		} else if (form instanceof ListStruct) {
			analyzedForm = ListStructAnalyzer.INSTANCE.analyze((ListStruct) form);
		}
		return analyzedForm;
	}

	/*
	 *********************************************************
	 * Analyzers
	 *********************************************************
	 */

	/*
	 *********************************************************
	 * Special Operator Analyzers
	 *********************************************************
	 */
/*
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
*/
	/*
	 *********************************************************
	 * OTHER STUFF
	 *********************************************************
	 */
/*
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
*/

	private LispStruct saSetClosureDepth(final LispStruct form, int depth) {
		if (!form.equals(NullStruct.INSTANCE) && (form instanceof ListStruct)) {

			final ListStruct workingList = (ListStruct) form;
			// this may be the start of a lambda or let expr
			final LispStruct theCar = workingList.getFirst();
			if (theCar instanceof ListStruct) {
				final LispStruct test = ((ListStruct) theCar).getFirst();
				if (test.equals(SpecialOperator.LAMBDA_MARKER) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.LABELS) || test.equals(SpecialOperator.FLET) || test.equals(SpecialOperator.MACRO_MARKER)) {
					final ListStruct theEnv = (ListStruct) theCar;
					// it is, so see if there's a closure defined
					// Get the current closure entry
					final ConsStruct closure = (ConsStruct) AssocFunction.funcall(KeywordOld.Closure, theEnv.getRest());
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
										new ConsStruct(KeywordOld.Depth, new IntegerStruct(BigInteger.valueOf(depth))),
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
		return form;
	}

}
