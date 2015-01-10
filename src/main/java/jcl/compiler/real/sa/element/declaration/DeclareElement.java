/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.declaration;

import jcl.compiler.real.sa.element.Element;

import java.util.ArrayList;
import java.util.List;

public class DeclareElement implements Element {

	private List<SpecialDeclarationElement> specialDeclarationElements;

	public List<SpecialDeclarationElement> getSpecialDeclarationElements() {
		if (specialDeclarationElements == null) {
			specialDeclarationElements = new ArrayList<>();
		}
		return specialDeclarationElements;
	}
}
