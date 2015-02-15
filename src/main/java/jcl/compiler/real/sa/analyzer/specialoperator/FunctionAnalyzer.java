package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FunctionElement;
import jcl.compiler.real.element.specialoperator.LambdaFunctionElement;
import jcl.compiler.real.element.specialoperator.SymbolFunctionElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.lambda.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.util.InstanceOf;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private LexicalSymbolStructAnalyzer lexicalSymbolStructAnalyzer;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public FunctionElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();

		return InstanceOf.when(second)
		                 .isInstanceOf(SymbolStruct.class).thenReturn(e -> analyzeFunctionSymbol(analyzer, e, analysisBuilder))
		                 .isInstanceOf(ListStruct.class).thenReturn(e -> analyzeFunctionList(analyzer, e, analysisBuilder))
		                 .otherwise(e -> {
			                 throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + e);
		                 });
	}

	private FunctionElement analyzeFunctionSymbol(final SemanticAnalyzer analyzer, final SymbolStruct<?> functionSymbol, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
//		final Environment currentEnvironment = environmentStack.peek();
		final Environment currentLexicalEnvironment = environmentStack.getCurrentLexicalEnvironment();

		final Environment bindingEnvironment
				= EnvironmentAccessor.getBindingEnvironment(functionSymbol, currentLexicalEnvironment, Marker.FUNCTION_MARKERS);

		final boolean missingFunctionSymbolBinding = !bindingEnvironment.hasLexicalBinding(functionSymbol);

		final SymbolElement<?> functionSymbolSE;
		if (missingFunctionSymbolBinding) {
			functionSymbolSE = lexicalSymbolStructAnalyzer.analyze(analyzer, functionSymbol, analysisBuilder);
		} else {
			functionSymbolSE = new SymbolElement<>(functionSymbol);
		}

		return new SymbolFunctionElement(functionSymbolSE);
	}

	private FunctionElement analyzeFunctionList(final SemanticAnalyzer analyzer, final ListStruct functionList, final AnalysisBuilder analysisBuilder) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
		}

		final LambdaElement lambdaElement = lambdaAnalyzer.analyze(analyzer, functionList, analysisBuilder);
		return new LambdaFunctionElement(lambdaElement);
	}
}
