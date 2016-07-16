/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Map;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;

public class TagbodyStruct extends CompilerSpecialOperatorStruct {

	private final Map<GoStruct<?>, PrognStruct> tagbodyForms;

	public TagbodyStruct(final Map<GoStruct<?>, PrognStruct> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<GoStruct<?>, PrognStruct> getTagbodyForms() {
		return tagbodyForms;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(TAGBODY");

		for (final Map.Entry<GoStruct<?>, PrognStruct> entry : tagbodyForms.entrySet()) {
			builder.append(' ');

			final GoStruct<?> key = entry.getKey();
			if (key instanceof GoIntegerStruct) {
				final IntegerStruct tag = ((GoIntegerStruct) key).getTag();
				final String printedKey = tag.toString();
				builder.append(printedKey);
			} else {
				final SymbolStruct tag = (SymbolStruct) key.getTag();
				if (!tag.getName().startsWith("Tag-")) {
					final String printedKey = tag.toString();
					builder.append(printedKey);
				}
			}

			builder.append(' ');

			final PrognStruct value = entry.getValue();
			final String printedValue = value.toString();
			builder.append(printedValue);
		}

		builder.append(')');

		return builder.toString();
	}
}
