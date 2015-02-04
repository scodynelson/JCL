package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.lambdalist.RestBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.BlockAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.CatchAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.EvalWhenAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.FletAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.FunctionAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.GoAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.IfAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.LabelsAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.LetAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.LetStarAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.LocallyAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.MacroletAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.MultipleValueCallAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.MultipleValueProg1Analyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.PrognAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.ProgvAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.QuoteAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.ReturnFromAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SetqAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SymbolMacroletAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.TagbodyAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.TheAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.ThrowAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.UnwindProtectAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.compiler.DefstructAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.special.LambdaAnalyzer;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.FunctionCallElement;
import jcl.compiler.real.element.LambdaFunctionCallElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LambdaElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.util.InstanceOf;
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
public class ListStructAnalyzer implements Analyzer<Element, ListStruct> {

	private static final long serialVersionUID = 5454983196467731873L;

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
	private transient ApplicationContext context;

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.equals(NullStruct.INSTANCE)) {
			return NullElement.INSTANCE;
		}

		final LispStruct first = input.getFirst();

		return InstanceOf.when(first)
		                 .isInstanceOf(SymbolStruct.class).thenReturn(analyzeFunctionCall(analyzer, input, analysisBuilder))
		                 .isInstanceOf(ListStruct.class).thenReturn(e -> analyzeLambdaFunctionCall(analyzer, input, analysisBuilder, e))
		                 .otherwise(e -> {
			                 throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		                 });
	}

	private Element analyzeFunctionCall(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, currentLexicalEnvironment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		return InstanceOf.when(expandedForm)
		                 .isInstanceOf(ConsStruct.class).thenReturn(e -> analyzedExpandedFormListFunctionCall(analyzer, analysisBuilder, e))
		                 .isInstanceOf(NullStruct.class).thenReturn(NullElement.INSTANCE)
		                 .otherwise(analyzer.analyzeForm(expandedForm, analysisBuilder));
	}

	private Element analyzedExpandedFormListFunctionCall(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder, final ListStruct expandedForm) {

		final LispStruct expandedFormListFirst = expandedForm.getFirst();
		final ListStruct functionArguments = expandedForm.getRest();

		return InstanceOf.when(expandedFormListFirst)
		                 .isInstanceOf(SpecialOperator.class).thenReturn(e -> analyzeSpecialOperator(analyzer, analysisBuilder, expandedForm, e))
		                 .isInstanceOf(SymbolStruct.class).thenReturn(e -> analyzeFunctionCall(analyzer, analysisBuilder, e, functionArguments))
		                 .otherwise(e -> {
			                 throw new ProgramErrorException("LIST ANALYZER: First element of expanded form must be a SpecialOperator or of type SymbolStruct. Got: " + e);
		                 });
	}

	private Element analyzeSpecialOperator(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder,
	                                       final ListStruct expandedForm, final SpecialOperator specialOperator) {

		final Class<? extends SpecialOperatorAnalyzer> strategy = STRATEGIES.get(specialOperator);
		if (strategy == null) {
			throw new ProgramErrorException("LIST ANALYZER: SpecialOperator symbol supplied is not supported: " + specialOperator);
		}

		final Analyzer<? extends Element, ListStruct> strategyBean = context.getBean(strategy);
		return strategyBean.analyze(analyzer, expandedForm, analysisBuilder);
	}

	private static FunctionCallElement analyzeFunctionCall(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder,
	                                                       final SymbolStruct<?> functionSymbol, final ListStruct functionArguments) {

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
			// Function is defined
			undefinedFunctions.remove(functionSymbol);

			final String functionName = functionSymbol.getName();
			final OrdinaryLambdaListBindings lambdaListBindings = function.getLambdaListBindings();
			validateFunctionArguments(functionName, lambdaListBindings, functionArgumentsList);
		}

		final List<Element> analyzedFunctionArguments = new ArrayList<>(functionArgumentsList.size());

		for (final LispStruct functionArgument : functionArgumentsList) {
			final Element analyzedFunctionArgument = analyzer.analyzeForm(functionArgument, analysisBuilder);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final boolean hasFunctionBinding = hasFunctionBinding(currentLexicalEnvironment, functionSymbol);

		final SymbolElement<?> functionSymbolSE = new SymbolElement<>(functionSymbol);
		return new FunctionCallElement(hasFunctionBinding, functionSymbolSE, analyzedFunctionArguments);
	}

	private static boolean hasFunctionBinding(final LexicalEnvironment lexicalEnvironment, final SymbolStruct<?> variable) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		boolean hasFunctionBinding = false;

		while (!currentLexicalEnvironment.equals(LexicalEnvironment.NULL)) {
			if (currentLexicalEnvironment.hasBinding(variable)) {
				hasFunctionBinding = true;
				break;
			}
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return hasFunctionBinding;
	}

	private static void validateFunctionArguments(final String functionName, final OrdinaryLambdaListBindings lambdaListBindings,
	                                              final List<LispStruct> functionArguments) {

		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		LispStruct nextArgument = null;

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding ignored : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("LIST ANALYZER: Too few arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at least " + requiredBindings.size() + " required.");
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
						throw new ProgramErrorException("LIST ANALYZER: Keyword argument not found in '" + functionName + "' function definition: " + argumentKey);
					}
				} else {
					throw new ProgramErrorException("LIST ANALYZER: Expected Keyword argument for call to '" + functionName + " was: " + nextArgument);
				}
			} else if (restBinding != null) {
				functionArgumentsIterator.next();
			} else {
				throw new ProgramErrorException("LIST ANALYZER: Too many arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at most " + requiredBindings.size() + " accepted.");
			}
		}
	}

	private LambdaFunctionCallElement analyzeLambdaFunctionCall(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder, final ListStruct functionList) {
		// ex ((lambda (x) (+ x 1)) 3)

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("LIST ANALYZER: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + functionListFirst);
		}

		final LambdaElement lambdaAnalyzed = lambdaAnalyzer.analyze(analyzer, functionList, analysisBuilder);
		final OrdinaryLambdaListBindings lambdaListBindings = lambdaAnalyzed.getLambdaListBindings();

		final ListStruct functionArguments = input.getRest();
		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		validateFunctionArguments("Anonymous Lambda", lambdaListBindings, functionArgumentsList);

		final List<Element> analyzedFunctionArguments = new ArrayList<>(functionArgumentsList.size());

		for (final LispStruct functionArgument : functionArgumentsList) {
			final Element analyzedFunctionArgument = analyzer.analyzeForm(functionArgument, analysisBuilder);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		return new LambdaFunctionCallElement(lambdaAnalyzed, analyzedFunctionArguments);
	}
}
