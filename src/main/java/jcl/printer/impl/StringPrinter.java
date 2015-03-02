package jcl.printer.impl;

import jcl.printer.PrinterVariables;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.struct.SyntaxType;

import java.util.List;

public abstract class StringPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = -1460076608687611503L;

	@Override
	public String print(final O object) {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getValue().booleanValue();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final List<Integer> contents = getCodePoints(object);
		final int amountToPrint = getAmountToPrint(object, contents);

		for (int i = 0; i < amountToPrint; i++) {
			final int codePoint = contents.get(i);

			final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);
			if ((codePoint == '"') || (syntaxType == SyntaxType.SINGLE_ESCAPE)) {
				stringBuilder.append('\\');
			}
			stringBuilder.appendCodePoint(codePoint);
		}

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}

	protected abstract List<Integer> getCodePoints(O object);

	protected abstract int getAmountToPrint(O object, List<Integer> contents);
}
