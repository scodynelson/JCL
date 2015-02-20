/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.characters.CharacterStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class CharacterElement implements Element {

	private static final long serialVersionUID = 8285480869779461957L;

	private final CharacterStruct characterStruct;

	public CharacterElement(final CharacterStruct characterStruct) {
		this.characterStruct = characterStruct;
	}

	public CharacterStruct getCharacterStruct() {
		return characterStruct;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
