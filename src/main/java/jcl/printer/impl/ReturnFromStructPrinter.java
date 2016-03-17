package jcl.printer.impl;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.ReturnFromStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromStructPrinter implements LispPrinter<ReturnFromStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final ReturnFromStruct object) {
		final StringBuilder builder = new StringBuilder("(RETURN-FROM ");

		final SymbolStruct name = object.getName();
		final String namePrinted = printer.print(name);
		builder.append(namePrinted);

		builder.append(' ');

		final LispStruct result = object.getResult();
		final String formsPrinted = printer.print(result);
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}
}
