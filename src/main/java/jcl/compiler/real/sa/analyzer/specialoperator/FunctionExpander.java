package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.LambdaExpander;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionExpander extends MacroFunctionExpander<CompilerFunctionStruct> {

	private static final long serialVersionUID = -8290125563768560922L;

	@Autowired
	private LambdaExpander lambdaExpander;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.FUNCTION;
	}

	@Override
	public CompilerFunctionStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSizeExact(form, 2, "FUNCTION");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		validator.validateObjectTypes(second, "FUNCTION", "FUNCTION ARGUMENT", SymbolStruct.class, ListStruct.class);

		if (second instanceof SymbolStruct) {
			return new SymbolCompilerFunctionStruct((SymbolStruct<?>) second);
		} else {
			return analyzeFunctionList((ListStruct) second, environment);
		}
	}

	private CompilerFunctionStruct analyzeFunctionList(final ListStruct functionList, final Environment environment) {

		final LispStruct functionListFirst = functionList.getFirst();

		if (!SpecialOperatorStruct.LAMBDA.equals(functionListFirst)) {
			final String printedObject = printer.print(functionListFirst);
			throw new ProgramErrorException("FUNCTION: First element of list argument must be the symbol 'LAMBDA'. Got: " + printedObject);
		}

		final LambdaStruct analyzedLambda = lambdaExpander.expand(functionList, environment);
		return new LambdaCompilerFunctionStruct(analyzedLambda);
	}
}
