package jcl.printer.impl;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.SetqStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqStructPrinter implements LispPrinter<SetqStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final SetqStruct object) {
		final StringBuilder builder = new StringBuilder("(SETQ");

		final List<SetqStruct.SetqPair> setqPairs = object.getSetqPairs();
		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			builder.append(' ');

			final SymbolStruct var = setqPair.getVar();
			final String varPrinted = printer.print(var);
			builder.append(varPrinted);

			builder.append(' ');

			final LispStruct form = setqPair.getForm();
			final String formPrinted = printer.print(form);
			builder.append(formPrinted);
		}

		builder.append(')');

		return builder.toString();
	}
}
