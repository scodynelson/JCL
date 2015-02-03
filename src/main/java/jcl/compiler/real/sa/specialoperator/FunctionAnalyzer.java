package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.FunctionElement;
import jcl.compiler.real.sa.element.LambdaElement;
import jcl.compiler.real.sa.element.LambdaFunctionElement;
import jcl.compiler.real.sa.element.SymbolFunctionElement;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Stack;

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

		if (second instanceof SymbolStruct) {
			return analyzeFunctionSymbol(analysisBuilder, (SymbolStruct) second);
		} else if (second instanceof ListStruct) {
			return analyzeFunctionList(analyzer, analysisBuilder, (ListStruct) second);
		} else {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}
	}

	private SymbolFunctionElement analyzeFunctionSymbol(final AnalysisBuilder analysisBuilder, final SymbolStruct<?> functionSymbol) {

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final LexicalEnvironment bindingLexicalEnvironment = getBindingEnvironment(currentLexicalEnvironment, functionSymbol);

		final boolean missingFunctionSymbolBinding = !bindingLexicalEnvironment.hasBinding(functionSymbol);
		if (missingFunctionSymbolBinding) {
			lexicalSymbolStructAnalyzer.analyzeSymbol(functionSymbol, analysisBuilder);
		}

		return new SymbolFunctionElement(functionSymbol);
	}

	private static LexicalEnvironment getBindingEnvironment(final LexicalEnvironment lexicalEnvironment, final SymbolStruct<?> variable) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		while (!currentLexicalEnvironment.equals(LexicalEnvironment.NULL)) {

			final Marker marker = currentLexicalEnvironment.getMarker();
			if (Marker.FUNCTION_MARKERS.contains(marker)) {

				final boolean hasBinding = currentLexicalEnvironment.hasBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}

	private LambdaFunctionElement analyzeFunctionList(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder, final ListStruct functionList) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
		}

		final LambdaElement lambdaElement = lambdaAnalyzer.analyze(analyzer, functionList, analysisBuilder);
		return new LambdaFunctionElement(lambdaElement);
	}
}
