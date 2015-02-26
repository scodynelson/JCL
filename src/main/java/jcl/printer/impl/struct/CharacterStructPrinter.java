/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.characters.CharacterStruct;
import jcl.printer.impl.CharacterPrinter;
import org.springframework.stereotype.Component;

@Component
public class CharacterStructPrinter extends CharacterPrinter<CharacterStruct> {

	@Override
	protected int getCodePoint(final CharacterStruct object) {
		return object.getCodePoint();
	}
}
