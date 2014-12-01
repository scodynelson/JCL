package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.arrays.ArrayStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	private final Stack<Environment> environmentStack = new Stack<>();
	private final Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private final Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());
	private int bindingsPosition;
	private int closureDepth;

	private final Stack<SymbolStruct<?>> blockStack = new Stack<>();
	private final Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack = new Stack<>();

	// eval-when processing modes
	private boolean topLevelMode;

	@Autowired
	private ListStructAnalyzer listStructAnalyzer;

	@Autowired
	private SymbolStructAnalyzer symbolStructAnalyzer;

	@Autowired
	private ArrayStructAnalyzer arrayStructAnalyzer;

	SemanticAnalyzerImpl() {
		initialize();
	}

	private void initialize() {
		//create the global environment
		environmentStack.clear();
		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());

		functionNameStack.clear();
		functionNameStack.push(null);

		undefinedFunctions.clear();
		bindingsPosition = 0;

		blockStack.clear();
		tagbodyStack.clear();

		topLevelMode = true;
	}

	@Override
	public LispStruct analyze(final LispStruct form) {
		initialize();

		final LispStruct lambdaForm = wrapFormInLambda(form);
		final LispStruct analyzedForm = analyzeForm(lambdaForm);

		// now see if we have any functions still undefined
		for (final SymbolStruct<?> undefinedFunction : undefinedFunctions) {
			if (undefinedFunction.getFunction() == null) {
				// TODO: Remove defined functions at some other point so we don't have to do the check here???
				LOGGER.warn("; Warning: no function or macro function defined for ");
				if (undefinedFunction.getSymbolPackage() != null) {
					LOGGER.warn("{}::{}\n", undefinedFunction.getSymbolPackage().getName(), undefinedFunction.getName());
				} else {
					LOGGER.warn("#:{}\n", undefinedFunction.getName());
				}
			}
		}

		return analyzedForm;
	}

	@Override
	public LispStruct analyzeForm(final LispStruct form) {

		LispStruct analyzedForm = form;
		if (form instanceof ListStruct) {
			analyzedForm = listStructAnalyzer.analyze((ListStruct) form, this);
		} else if (form instanceof SymbolStruct) {
			analyzedForm = symbolStructAnalyzer.analyze((SymbolStruct<?>) form, this);
		} else if (form instanceof ArrayStruct) {
			analyzedForm = arrayStructAnalyzer.analyze((ArrayStruct<?>) form, this);
		}
		return analyzedForm;
	}

	private static LispStruct wrapFormInLambda(final LispStruct form) {

		LispStruct lambdaForm = form;
		if (form instanceof ListStruct) {
			final ListStruct formList = (ListStruct) form;
			final LispStruct firstOfFormList = formList.getFirst();
			if (!(firstOfFormList instanceof SymbolStruct) || !firstOfFormList.equals(SpecialOperator.LAMBDA)) {
				lambdaForm = ListStruct.buildProperList(SpecialOperator.LAMBDA, NullStruct.INSTANCE, form);
			}
		} else {
			lambdaForm = ListStruct.buildProperList(SpecialOperator.LAMBDA, NullStruct.INSTANCE, form);
		}

		return lambdaForm;
	}

	@Override
	public Stack<Environment> getEnvironmentStack() {
		return environmentStack;
	}

	@Override
	public Set<SymbolStruct<?>> getUndefinedFunctions() {
		return undefinedFunctions;
	}

	@Override
	public Stack<SymbolStruct<?>> getFunctionNameStack() {
		return functionNameStack;
	}

	@Override
	public int getBindingsPosition() {
		return bindingsPosition;
	}

	@Override
	public void setBindingsPosition(final int bindingsPosition) {
		this.bindingsPosition = bindingsPosition;
	}

	@Override
	public int getClosureDepth() {
		return closureDepth;
	}

	@Override
	public void setClosureDepth(final int closureDepth) {
		this.closureDepth = closureDepth;
	}

	@Override
	public Stack<SymbolStruct<?>> getBlockStack() {
		return blockStack;
	}

	@Override
	public Stack<Map<LispStruct, SymbolStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	@Override
	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	@Override
	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
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
}
