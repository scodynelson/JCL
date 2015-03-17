/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.util.List;
import java.util.stream.Collectors;

import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.struct.SyntaxType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class StringStructPrinter implements LispPrinter<StringStruct> {

	private static final long serialVersionUID = -1997189358891781811L;

	@Override
	public String print(final StringStruct object) {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getValue().booleanValue();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final List<Integer> contents = object.getContents()
		                                     .stream()
		                                     .map(CharacterStruct::getCodePoint)
		                                     .collect(Collectors.toList());

		final Integer fillPointer = object.getFillPointer();
		final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

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
