package jcl.printer.impl;

import jcl.compiler.struct.specialoperator.go.GoSymbolStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoSymbolStructPrinter implements LispPrinter<GoSymbolStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final GoSymbolStruct object) {
		final StringBuilder builder = new StringBuilder("(GO ");

		final SymbolStruct tag = object.getTag();
		final String printedTag = printer.print(tag);
		builder.append(printedTag);

		builder.append(')');

		return builder.toString();
	}
}
