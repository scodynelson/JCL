/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.characters.CharacterStruct;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class CharacterStructPrinter implements LispPrinter<CharacterStruct> {

	private static final long serialVersionUID = 5997216783690456319L;

	@Override
	public String print(final CharacterStruct object) {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getValue().booleanValue();

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

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
