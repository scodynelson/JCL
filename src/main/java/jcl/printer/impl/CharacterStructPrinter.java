/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.characters.CharacterStruct;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import org.springframework.stereotype.Component;

@Component
public class CharacterStructPrinter implements LispPrinter<CharacterStruct> {

	@Override
	public String print(final CharacterStruct object) {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append("#\\");
		}

		final int codePoint = object.getCodePoint();

		if (Character.isWhitespace(codePoint)) {
			stringBuilder.append(Character.getName(codePoint));
		} else {
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}
}
