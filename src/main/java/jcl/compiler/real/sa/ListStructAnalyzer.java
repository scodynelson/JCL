package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironmentLispStruct;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.lambdalist.RestBinding;
import jcl.compiler.real.sa.specialoperator.BlockAnalyzer;
import jcl.compiler.real.sa.specialoperator.CatchAnalyzer;
import jcl.compiler.real.sa.specialoperator.EvalWhenAnalyzer;
import jcl.compiler.real.sa.specialoperator.FletAnalyzer;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.compiler.real.sa.specialoperator.GoAnalyzer;
import jcl.compiler.real.sa.specialoperator.IfAnalyzer;
import jcl.compiler.real.sa.specialoperator.LabelsAnalyzer;
import jcl.compiler.real.sa.specialoperator.LetAnalyzer;
import jcl.compiler.real.sa.specialoperator.LetStarAnalyzer;
import jcl.compiler.real.sa.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.specialoperator.LocallyAnalyzer;
import jcl.compiler.real.sa.specialoperator.MacroletAnalyzer;
import jcl.compiler.real.sa.specialoperator.MultipleValueCallAnalyzer;
import jcl.compiler.real.sa.specialoperator.MultipleValueProg1Analyzer;
import jcl.compiler.real.sa.specialoperator.PrognAnalyzer;
import jcl.compiler.real.sa.specialoperator.ProgvAnalyzer;
import jcl.compiler.real.sa.specialoperator.QuoteAnalyzer;
import jcl.compiler.real.sa.specialoperator.ReturnFromAnalyzer;
import jcl.compiler.real.sa.specialoperator.SetqAnalyzer;
import jcl.compiler.real.sa.specialoperator.SpecialOperatorAnalyzer;
import jcl.compiler.real.sa.specialoperator.SymbolMacroletAnalyzer;
import jcl.compiler.real.sa.specialoperator.TagbodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.TheAnalyzer;
import jcl.compiler.real.sa.specialoperator.ThrowAnalyzer;
import jcl.compiler.real.sa.specialoperator.UnwindProtectAnalyzer;
import jcl.compiler.real.sa.specialoperator.compiler.DefstructAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

@Component
public class ListStructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	private static final Map<SpecialOperator, Class<? extends SpecialOperatorAnalyzer>> STRATEGIES = new HashMap<>();

	static {
		STRATEGIES.put(SpecialOperator.BLOCK, BlockAnalyzer.class);
		STRATEGIES.put(SpecialOperator.CATCH, CatchAnalyzer.class);
		STRATEGIES.put(SpecialOperator.EVAL_WHEN, EvalWhenAnalyzer.class);
		STRATEGIES.put(SpecialOperator.FLET, FletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.FUNCTION, FunctionAnalyzer.class);
		STRATEGIES.put(SpecialOperator.GO, GoAnalyzer.class);
		STRATEGIES.put(SpecialOperator.IF, IfAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LABELS, LabelsAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LET, LetAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LET_STAR, LetStarAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LOAD_TIME_VALUE, LoadTimeValueAnalyzer.class);
		STRATEGIES.put(SpecialOperator.LOCALLY, LocallyAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MACROLET, MacroletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_CALL, MultipleValueCallAnalyzer.class);
		STRATEGIES.put(SpecialOperator.MULTIPLE_VALUE_PROG1, MultipleValueProg1Analyzer.class);
		STRATEGIES.put(SpecialOperator.PROGN, PrognAnalyzer.class);
		STRATEGIES.put(SpecialOperator.PROGV, ProgvAnalyzer.class);
		STRATEGIES.put(SpecialOperator.QUOTE, QuoteAnalyzer.class);
		STRATEGIES.put(SpecialOperator.RETURN_FROM, ReturnFromAnalyzer.class);
		STRATEGIES.put(SpecialOperator.SETQ, SetqAnalyzer.class);
		STRATEGIES.put(SpecialOperator.SYMBOL_MACROLET, SymbolMacroletAnalyzer.class);
		STRATEGIES.put(SpecialOperator.TAGBODY, TagbodyAnalyzer.class);
		STRATEGIES.put(SpecialOperator.THE, TheAnalyzer.class);
		STRATEGIES.put(SpecialOperator.THROW, ThrowAnalyzer.class);
		STRATEGIES.put(SpecialOperator.UNWIND_PROTECT, UnwindProtectAnalyzer.class);

//		STRATEGIES.put(SpecialOperator.DECLARE, DeclareAnalyzer.INSTANCE);
		STRATEGIES.put(SpecialOperator.LAMBDA, LambdaAnalyzer.class);

		STRATEGIES.put(SpecialOperator.DEFSTRUCT, DefstructAnalyzer.class);
	}

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Autowired
	private ApplicationContext context;

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.equals(NullStruct.INSTANCE)) {
			return input;
		}

		final LispStruct first = input.getFirst();
		if (!(first instanceof SymbolStruct) && !(first instanceof ListStruct)) {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}

		if (first instanceof SymbolStruct) {
			final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
			final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, environmentStack.peek());
			final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

			if (expandedForm instanceof ConsStruct) {
				final ListStruct expandedFormList = (ListStruct) expandedForm;

				final LispStruct expandedFormListFirst = expandedFormList.getFirst();
				if (expandedFormListFirst instanceof SpecialOperator) {
					return analyzeSpecialOperator(analyzer, expandedFormList, analysisBuilder);
				} else if (expandedFormListFirst instanceof SymbolStruct) {
					final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) expandedFormListFirst;
					final ListStruct functionArguments = expandedFormList.getRest();
					return analyzeFunctionCall(analyzer, analysisBuilder, functionSymbol, functionArguments);
				} else {
					throw new ProgramErrorException("SA LIST: First element of expanded form must be a SpecialOperator or of type SymbolStruct. Got: " + expandedFormListFirst);
				}
			} else if (expandedForm instanceof NullStruct) {
				return expandedForm;
			} else {
				return analyzer.analyzeForm(expandedForm, analysisBuilder);
			}
		}

		// ex ((lambda (x) (+ x 1)) 3)
		final ListStruct firstAsList = (ListStruct) first;

		final LispStruct firstOfFirstList = firstAsList.getFirst();
		if (firstOfFirstList.equals(SpecialOperator.LAMBDA)) {
			final LambdaEnvironmentLispStruct lambdaAnalyzed = lambdaAnalyzer.analyze(analyzer, firstAsList, analysisBuilder);
			final ListStruct functionArguments = input.getRest();
			return analyzedLambdaFunctionCall(analyzer, analysisBuilder, lambdaAnalyzed, functionArguments);
		} else {
			throw new ProgramErrorException("SA LIST: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + firstOfFirstList);
		}
	}

	private LispStruct analyzeSpecialOperator(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final SpecialOperator specialOperator = (SpecialOperator) input.getFirst();

		final Class<? extends SpecialOperatorAnalyzer> strategy = STRATEGIES.get(specialOperator);
		final Analyzer<? extends LispStruct, ListStruct> strategyBean = context.getBean(strategy);

		if (strategy == null) {
			throw new ProgramErrorException("SpecialOperator symbol supplied is not supported: " + specialOperator);
		}
		return strategyBean.analyze(analyzer, input, analysisBuilder);
	}

	private static ListStruct analyzedLambdaFunctionCall(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder,
	                                                     final LambdaEnvironmentLispStruct lambdaAnalyzed, final ListStruct functionArguments) {

		final List<LispStruct> analyzedFunctionList = new ArrayList<>();
		analyzedFunctionList.add(lambdaAnalyzed);

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final OrdinaryLambdaListBindings lambdaListBindings = lambdaAnalyzed.getLambdaListBindings();
		validateFunctionArguments("Anonymous Lambda", lambdaListBindings, functionArgumentsList);

		for (final LispStruct currentArgument : functionArgumentsList) {
			final LispStruct analyzedArgument = analyzer.analyzeForm(currentArgument, analysisBuilder);
			analyzedFunctionList.add(analyzedArgument);
		}

		return ListStruct.buildProperList(analyzedFunctionList);
	}

	private static ListStruct analyzeFunctionCall(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder,
	                                              final SymbolStruct<?> functionSymbol, final ListStruct functionArguments) {

		final List<LispStruct> analyzedFunctionList = new ArrayList<>();

		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
		final boolean hasFunctionBinding = hasFunctionBinding(environmentStack.peek(), functionSymbol);
		if (hasFunctionBinding) {
			// Recursive call
			analyzedFunctionList.add(SpecialOperator.TAIL_RECURSION);
		}
		analyzedFunctionList.add(functionSymbol);

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final Set<SymbolStruct<?>> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		final FunctionStruct function = functionSymbol.getFunction();
		if (function == null) {
			final Stack<SymbolStruct<?>> functionNameStack = analysisBuilder.getFunctionNameStack();

			if (functionNameStack.contains(functionSymbol)) {
				// Function is undefined, but name exists on the stack to be created
				undefinedFunctions.remove(functionSymbol);
			} else {
				// Add this as a possible undefined function
				undefinedFunctions.add(functionSymbol);
			}
		} else {
			// Function is defined.
			undefinedFunctions.remove(functionSymbol);

			final String functionName = functionSymbol.getName();
			final OrdinaryLambdaListBindings lambdaListBindings = function.getLambdaListBindings();
			validateFunctionArguments(functionName, lambdaListBindings, functionArgumentsList);
		}

		for (final LispStruct currentArgument : functionArgumentsList) {
			final LispStruct analyzedArgument = analyzer.analyzeForm(currentArgument, analysisBuilder);
			analyzedFunctionList.add(analyzedArgument);
		}

		return ListStruct.buildProperList(analyzedFunctionList);
	}

	private static boolean hasFunctionBinding(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		boolean hasFunctionBinding = false;

		while (!currentEnvironment.equals(Environment.NULL)) {
			if (currentEnvironment.hasBinding(variable)) {
				hasFunctionBinding = true;
				break;
			}
			currentEnvironment = currentEnvironment.getParent();
		}

		return hasFunctionBinding;
	}

	private static void validateFunctionArguments(final String functionName, final OrdinaryLambdaListBindings lambdaListBindings, final List<LispStruct> functionArguments) {
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		LispStruct nextArgument = null;

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding ignored : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("SA LIST: Too few arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at least " + requiredBindings.size() + " required.");
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding ignored : optionalBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				break;
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		final List<KeywordSymbolStruct> keys = new ArrayList<>(keyBindings.size());
		for (final KeyBinding keyBinding : keyBindings) {
			final KeywordSymbolStruct key = keyBinding.getKeyName();
			keys.add(key);
		}

		final RestBinding restBinding = lambdaListBindings.getRestBinding();

		while (functionArgumentsIterator.hasNext()) {
			if (!keyBindings.isEmpty()) {
				if (nextArgument instanceof KeywordSymbolStruct) {
					final KeywordSymbolStruct argumentKey = (KeywordSymbolStruct) nextArgument;
					if (keys.contains(argumentKey)) {
						// Consume the next argument
						functionArgumentsIterator.next();
						nextArgument = functionArgumentsIterator.next();
					} else {
						throw new ProgramErrorException("SA LIST: Keyword argument not found in '" + functionName + "' function definition: " + argumentKey);
					}
				} else {
					throw new ProgramErrorException("SA LIST: Expected Keyword argument for call to '" + functionName + " was: " + nextArgument);
				}
			} else if (restBinding != null) {
				functionArgumentsIterator.next();
			} else {
				throw new ProgramErrorException("SA LIST: Too many arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at most " + requiredBindings.size() + " accepted.");
			}
		}
	}
}
