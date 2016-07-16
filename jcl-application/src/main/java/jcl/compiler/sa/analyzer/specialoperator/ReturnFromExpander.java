package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ReturnFromStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.TypeErrorException;
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
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.RETURN_FROM;
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // RETURN-FROM SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: 0. Expected between 1 and 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			final String printedObject = printer.print(first);
			throw new TypeErrorException("RETURN-FROM: NAME must be a Symbol. Got: " + printedObject);
		}
		final SymbolStruct name = (SymbolStruct) first;

		if (environment.getBlockStack().search(name) == -1) {
			final String printedObject = printer.print(name);
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + printedObject + " is visible.");
		}

		LispStruct third = null;
		if (iterator.hasNext()) {
			third = iterator.next();
		}
		if (iterator.hasNext()) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: 2. Expected between 1 and 2 arguments.");
		}

		final LispStruct result;
		if (third == null) {
			result = NILStruct.INSTANCE;
		} else {
			result = formAnalyzer.analyze(third, environment);
		}
		return new ReturnFromStruct(name, result);
	}
}
