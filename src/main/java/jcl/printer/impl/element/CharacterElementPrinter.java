/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.CharacterElement;
import jcl.printer.impl.CharacterPrinter;
import org.springframework.stereotype.Component;

@Component
public class CharacterElementPrinter extends CharacterPrinter<CharacterElement> {

	@Override
	protected int getCodePoint(final CharacterElement object) {
		return object.getCodePoint();
	}
}
