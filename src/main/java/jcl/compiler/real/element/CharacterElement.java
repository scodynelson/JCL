/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class CharacterElement implements SimpleElement {

	private static final long serialVersionUID = 8285480869779461957L;

	private final int codePoint;

	public CharacterElement(final int codePoint) {
		this.codePoint = codePoint;
	}

	public int getCodePoint() {
		return codePoint;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	@Override
	public LispStruct toLispStruct() {
		return new CharacterStruct(codePoint);
	}
}
