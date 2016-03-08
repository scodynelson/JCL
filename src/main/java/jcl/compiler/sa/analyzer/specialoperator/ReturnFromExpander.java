package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.ReturnFromStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromExpander extends MacroFunctionExpander<ReturnFromStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.RETURN_FROM;
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {
		final int formSize = validator.validateListFormSize(form, 2, 3, "RETURN-FROM");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getCar();
		final SymbolStruct name = validator.validateObjectType(second, "RETURN-FROM", "NAME", SymbolStruct.class);

		if (environment.getBlockStack().search(name) == -1) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + printedObject + " is visible.");
		}

		final LispStruct analyzedResult;
		if (formSize == 3) {
			final ListStruct formRestRest = formRest.getRest();

			final LispStruct result = formRestRest.getCar();
			analyzedResult = formAnalyzer.analyze(result, environment);
		} else {
			analyzedResult = NILStruct.INSTANCE;
		}

		return new ReturnFromStruct(name, analyzedResult);
	}
}
