package jcl.printer.impl;

import java.util.Map;

import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.TagbodyStruct;
import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyStructPrinter implements LispPrinter<TagbodyStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final TagbodyStruct object) {
		final StringBuilder builder = new StringBuilder("(TAGBODY");

		final Map<GoStruct<?>, PrognStruct> tagbodyForms = object.getTagbodyForms();

		for (final Map.Entry<GoStruct<?>, PrognStruct> entry : tagbodyForms.entrySet()) {
			builder.append(' ');

			final GoStruct<?> key = entry.getKey();
			if (key instanceof GoIntegerStruct) {
				final IntegerStruct tag = ((GoIntegerStruct) key).getTag();
				final String printedKey = printer.print(tag);
				builder.append(printedKey);
			} else {
				final SymbolStruct tag = (SymbolStruct) key.getTag();
				if (!tag.getName().startsWith("Tag-")) {
					final String printedKey = printer.print(tag);
					builder.append(printedKey);
				}
			}

			builder.append(' ');

			final PrognStruct value = entry.getValue();
			final String printedValue = printer.print(value);
			builder.append(printedValue);
		}

		builder.append(')');

		return builder.toString();
	}
}
