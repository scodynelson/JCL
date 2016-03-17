package jcl.printer.impl;

import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoIntegerStructPrinter implements LispPrinter<GoIntegerStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final GoIntegerStruct object) {
		final StringBuilder builder = new StringBuilder("(GO ");

		final IntegerStruct tag = object.getTag();
		final String printedTag = printer.print(tag);
		builder.append(printedTag);

		builder.append(')');

		return builder.toString();
	}
}
