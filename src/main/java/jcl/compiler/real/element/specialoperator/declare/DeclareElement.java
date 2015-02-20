/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.declare;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class DeclareElement implements Element {

	private static final long serialVersionUID = -7730761501615283012L;

	private final List<SpecialDeclarationElement> specialDeclarationElements = new ArrayList<>();

	public List<SpecialDeclarationElement> getSpecialDeclarationElements() {
		return specialDeclarationElements;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
