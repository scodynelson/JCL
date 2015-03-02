/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.printer.impl.StringPrinter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class StringStructPrinter extends StringPrinter<StringStruct> {

	private static final long serialVersionUID = -1997189358891781811L;

	@Override
	protected List<Integer> getCodePoints(final StringStruct object) {
		return object.getContents()
		             .stream()
		             .map(CharacterStruct::getCodePoint)
		             .collect(Collectors.toList());
	}

	@Override
	protected int getAmountToPrint(final StringStruct object, final List<Integer> contents) {
		final Integer fillPointer = object.getFillPointer();
		return (fillPointer == null) ? contents.size() : fillPointer;
	}
}
