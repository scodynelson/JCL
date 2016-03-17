package jcl.printer.impl;

import jcl.compiler.struct.specialoperator.BlockStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockStructPrinter implements LispPrinter<BlockStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final BlockStruct object) {
		final StringBuilder builder = new StringBuilder("(BLOCK ");

		final SymbolStruct name = object.getName();
		final String namePrinted = printer.print(name);
		builder.append(namePrinted);

		builder.append(' ');

		final PrognStruct forms = object.getForms();
		final String formsPrinted = printer.print(forms);
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}
}
