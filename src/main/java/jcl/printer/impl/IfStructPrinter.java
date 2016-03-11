package jcl.printer.impl;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.IfStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfStructPrinter implements LispPrinter<IfStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final IfStruct object) {
		final StringBuilder builder = new StringBuilder("(IF ");

		final LispStruct testForm = object.getTestForm();
		final String testFormPrinted = printer.print(testForm);
		builder.append(testFormPrinted);
		builder.append(' ');
		final LispStruct thenForm = object.getThenForm();
		final String thenFormPrinted = printer.print(thenForm);
		builder.append(thenFormPrinted);
		builder.append(' ');
		final LispStruct elseForm = object.getElseForm();
		final String elseFormPrinted = printer.print(elseForm);
		builder.append(elseFormPrinted);
		builder.append(')');

		return builder.toString();
	}
}
