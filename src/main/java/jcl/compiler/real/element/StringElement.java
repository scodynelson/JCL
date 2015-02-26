/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;
import java.util.stream.Collectors;

public class StringElement implements SequenceElement {

	private static final long serialVersionUID = -5291597078209527471L;

	private final String javaString;

	public StringElement(final String javaString) {
		this.javaString = javaString;
	}

	public String getJavaString() {
		return javaString;
	}

	@Override
	public List<CharacterElement> getElements() {
		return javaString.chars()
		                 .mapToObj(CharacterElement::new)
		                 .collect(Collectors.toList());
	}

	@Override
	public LispStruct toLispStruct() {
		return new StringStruct(javaString);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
