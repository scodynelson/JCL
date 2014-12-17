/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;
import java.util.Map;

public class TagbodyElement implements Element {

	private final Map<LispStruct, List<LispStruct>> tagbodyForms;

	public TagbodyElement(final Map<LispStruct, List<LispStruct>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}
}
