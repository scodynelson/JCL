package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.TheStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TheExpander extends MacroFunctionExpander<TheStruct> {

	private static final long serialVersionUID = 6723289642694216454L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the the macro function and adds it to the special operator 'the'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.THE.setMacroFunctionExpander(this);
	}

	@Override
	public TheStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + formSize + ". Expected 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct valueType = formRest.getFirst();
		if (!(valueType instanceof SymbolStruct) && !(valueType instanceof ListStruct)) {
			final String printedValueType = printer.print(valueType);
			throw new ProgramErrorException("THE: Type specifier must be a symbol or a list. Got: " + printedValueType);
		}
		// TODO: do we actually want to somehow factor in the 'TypeSpecifier' produced by the second value?

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct theForm = formRestRest.getFirst();
		final LispStruct theFormAnalyzed = formAnalyzer.analyze(theForm, environment);

		return new TheStruct(null, theFormAnalyzed);
	}
}
