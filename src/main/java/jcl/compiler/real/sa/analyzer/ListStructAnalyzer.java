package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.FunctionCallElement;
import jcl.compiler.real.element.LambdaFunctionCallElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.lambda.LambdaAnalyzer;
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
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

@Component
public class ListStructAnalyzer implements Analyzer<Element, ListStruct> {

	private static final long serialVersionUID = 5454983196467731873L;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Resource
	private Map<SpecialOperator, SpecialOperatorAnalyzer> specialOperatorAnalyzerStrategies;

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

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, currentEnvironment);
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

		final SpecialOperatorAnalyzer specialOperatorAnalyzer = specialOperatorAnalyzerStrategies.get(specialOperator);
		if (specialOperatorAnalyzer == null) {
			throw new ProgramErrorException("LIST ANALYZER: SpecialOperator symbol supplied is not supported: " + specialOperator);
		}

		return specialOperatorAnalyzer.analyze(analyzer, expandedForm, analysisBuilder);
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

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final boolean hasFunctionBinding = hasFunctionBinding(currentEnvironment, functionSymbol);

		final SymbolElement<?> functionSymbolSE = new SymbolElement<>(functionSymbol);
		return new FunctionCallElement(hasFunctionBinding, functionSymbolSE, analyzedFunctionArguments);
	}

	private static boolean hasFunctionBinding(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		boolean hasFunctionBinding = false;

		while (!currentEnvironment.equals(Environment.NULL)) {
			if (currentEnvironment.hasLexicalBinding(variable)) {
				hasFunctionBinding = true;
				break;
			}
			currentEnvironment = currentEnvironment.getParent();
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
