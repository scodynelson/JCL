/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.CharacterElement;
import jcl.compiler.real.element.StringElement;
import jcl.printer.impl.StringPrinter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class StringElementPrinter extends StringPrinter<StringElement> {

	@Override
	protected List<Integer> getCodePoints(final StringElement object) {
		return object.getElements()
		             .stream()
		             .map(CharacterElement::getCodePoint)
		             .collect(Collectors.toList());
	}

	@Override
	protected int getAmountToPrint(final StringElement object, final List<Integer> contents) {
		return contents.size();
	}
}
