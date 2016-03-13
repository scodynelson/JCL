package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.TheStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TheExpander extends MacroFunctionExpander<TheStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.THE;
	}

	@Override
	public TheStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // THE SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 0. Expected 2 arguments.");
		}
		final LispStruct valueType = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 1. Expected 2 arguments.");
		}
		final LispStruct theForm = iterator.next();

		if (!(theForm instanceof SymbolStruct) && !(theForm instanceof ListStruct)) {
			final String printedObject = printer.print(theForm);
			throw new TypeErrorException("THE: TYPE-SPECIFIER must be either a Symbol or a List. Got: " + printedObject);
		}

		if (iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 3. Expected 2 arguments.");
		}

		final LispStruct theFormAnalyzed = formAnalyzer.analyze(theForm, environment);
		return new TheStruct(null, theFormAnalyzed);
	}
}
