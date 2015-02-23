package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FunctionElement;
import jcl.compiler.real.element.specialoperator.LambdaFunctionElement;
import jcl.compiler.real.element.specialoperator.SymbolFunctionElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.LexicalSymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.lambda.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private LexicalSymbolAnalyzer lexicalSymbolAnalyzer;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public FunctionElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof SymbolStruct) {
			return analyzeFunctionSymbol(analyzer, (SymbolStruct<?>) second, analysisBuilder);
		} else if (second instanceof ListStruct) {
			return analyzeFunctionList(analyzer, (ListStruct) second, analysisBuilder);
		} else {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}
	}

	private FunctionElement analyzeFunctionSymbol(final SemanticAnalyzer analyzer, final SymbolStruct<?> functionSymbol, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final Environment bindingEnvironment
				= Environments.getInnerFunctionLexicalBindingEnvironment(currentEnvironment, functionSymbol);

		final boolean hasNoFunctionSymbolBinding = !bindingEnvironment.hasLexicalBinding(functionSymbol);

		final SymbolElement functionSymbolSE;
		if (hasNoFunctionSymbolBinding) {
			functionSymbolSE = lexicalSymbolAnalyzer.analyze(analyzer, functionSymbol, analysisBuilder);
		} else {
			functionSymbolSE = new SymbolElement(functionSymbol.getSymbolPackage().getName(), functionSymbol.getName()); // TODO: fix
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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
