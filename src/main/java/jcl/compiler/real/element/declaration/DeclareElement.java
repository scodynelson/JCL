/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.declaration;

import jcl.compiler.real.element.Element;

import java.util.ArrayList;
import java.util.List;

public class DeclareElement implements Element {

	private static final long serialVersionUID = -7730761501615283012L;

	private final List<SpecialDeclarationElement> specialDeclarationElements = new ArrayList<>();

	public List<SpecialDeclarationElement> getSpecialDeclarationElements() {
		return specialDeclarationElements;
	}
}
