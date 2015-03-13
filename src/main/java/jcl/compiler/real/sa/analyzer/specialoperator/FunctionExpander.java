package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.sa.analyzer.LambdaExpander;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
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
public class FunctionExpander extends MacroFunctionExpander<CompilerFunctionStruct> {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	@Autowired
	private LambdaExpander lambdaExpander;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.FUNCTION.setMacroFunctionExpander(this);
	}

	@Override
	public CompilerFunctionStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();
		final LispStruct second = inputRest.getFirst();

		if (second instanceof SymbolStruct) {
			return analyzeFunctionSymbol((SymbolStruct<?>) second, environment);
		} else if (second instanceof ListStruct) {
			return analyzeFunctionList((ListStruct) second, environment);
		} else {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}
	}

	private CompilerFunctionStruct analyzeFunctionSymbol(final SymbolStruct<?> functionSymbol, final Environment environment) {

		final Environment bindingEnvironment
				= Environments.getInnerFunctionLexicalBindingEnvironment(environment, functionSymbol);

		final boolean hasNoFunctionSymbolBinding = !bindingEnvironment.hasLexicalBinding(functionSymbol);

		SymbolStruct<?> functionSymbolAnalyzed = functionSymbol;
		if (hasNoFunctionSymbolBinding) {
			functionSymbolAnalyzed = symbolAnalyzer.analyzeLexical(functionSymbol, environment);
		}

		return new SymbolCompilerFunctionStruct(functionSymbolAnalyzed);
	}

	private CompilerFunctionStruct analyzeFunctionList(final ListStruct functionList, final Environment environment) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
		}

		final LambdaStruct lambdaElement = lambdaExpander.expand(functionList, environment);
		return new LambdaCompilerFunctionStruct(lambdaElement);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
