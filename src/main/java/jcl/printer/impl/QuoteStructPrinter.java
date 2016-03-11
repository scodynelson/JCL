package jcl.printer.impl;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteStructPrinter implements LispPrinter<QuoteStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final QuoteStruct object) {
		final StringBuilder builder = new StringBuilder("'");

		final LispStruct quotedObject = object.getObject();
		final String objectPrinted = printer.print(quotedObject);
		builder.append(objectPrinted);

		return builder.toString();
	}
}
