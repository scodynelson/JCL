package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.LambdaExpander;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionExpander extends MacroFunctionExpander<CompilerFunctionStruct> {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private LambdaExpander lambdaExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the function macro function and adds it to the special operator 'function'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.FUNCTION.setMacroFunctionExpander(this);
	}

	@Override
	public CompilerFunctionStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + formSize + ". Expected 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (second instanceof SymbolStruct) {
			return new SymbolCompilerFunctionStruct((SymbolStruct<?>) second);
		} else if (second instanceof ListStruct) {
			return analyzeFunctionList((ListStruct) second, environment);
		} else {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("FUNCTION: Function argument must be a symbol or a list. Got: " + printedObject);
		}
	}

	private CompilerFunctionStruct analyzeFunctionList(final ListStruct functionList, final Environment environment) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!SpecialOperator.LAMBDA.equals(functionListFirst)) {
			final String printedObject = printer.print(functionListFirst);
			throw new ProgramErrorException("FUNCTION: First element of list argument must be the symbol 'LAMBDA'. Got: " + printedObject);
		}

		final LambdaStruct analyzedLambda = lambdaExpander.expand(functionList, environment);
		return new LambdaCompilerFunctionStruct(analyzedLambda);
	}
}
