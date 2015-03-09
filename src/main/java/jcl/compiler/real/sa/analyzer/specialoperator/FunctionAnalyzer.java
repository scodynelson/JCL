package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.LexicalSymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.specialoperator.lambda.LambdaAnalyzer;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private LexicalSymbolAnalyzer lexicalSymbolAnalyzer;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.FUNCTION.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public CompilerFunctionStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final ListStruct inputRest = input.getRest();
		final LispStruct second = inputRest.getFirst();

		if (second instanceof SymbolStruct) {
			return analyzeFunctionSymbol((SymbolStruct<?>) second, analysisBuilder);
		} else if (second instanceof ListStruct) {
			return analyzeFunctionList((ListStruct) second, analysisBuilder);
		} else {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}
	}

	private CompilerFunctionStruct analyzeFunctionSymbol(final SymbolStruct<?> functionSymbol, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final Environment bindingEnvironment
				= Environments.getInnerFunctionLexicalBindingEnvironment(currentEnvironment, functionSymbol);

		final boolean hasNoFunctionSymbolBinding = !bindingEnvironment.hasLexicalBinding(functionSymbol);

		SymbolStruct<?> functionSymbolAnalyzed = functionSymbol;
		if (hasNoFunctionSymbolBinding) {
			functionSymbolAnalyzed = lexicalSymbolAnalyzer.analyze(functionSymbol, analysisBuilder);
		}

		return new SymbolCompilerFunctionStruct(functionSymbolAnalyzed);
	}

	private CompilerFunctionStruct analyzeFunctionList(final ListStruct functionList, final AnalysisBuilder analysisBuilder) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
		}

		final LambdaStruct lambdaElement = lambdaAnalyzer.analyze(functionList, analysisBuilder);
		return new LambdaCompilerFunctionStruct(lambdaElement);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
