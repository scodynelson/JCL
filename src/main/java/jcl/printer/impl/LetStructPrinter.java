package jcl.printer.impl;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.LetStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStructPrinter implements LispPrinter<LetStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final LetStruct object) {
		final StringBuilder builder = new StringBuilder("(LET (");

		final List<LetStruct.LetVar> vars = object.getVars();
		final String varsPrinted =
				vars.stream()
				    .map(var -> '(' + printer.print(var.getVar()) + ' ' + printer.print(var.getInitForm()) + ')')
				    .collect(Collectors.joining(" "));
		builder.append(varsPrinted);
		builder.append(") ");

		final PrognStruct forms = object.getForms();
		final List<LispStruct> formsList = forms.getForms();
		for (final LispStruct form : formsList) {
			final String formPrint = printer.print(form);
			builder.append(formPrint);
		}

		builder.append(')');

		return builder.toString();
	}
}
