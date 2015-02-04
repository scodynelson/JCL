/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.Element;

import java.util.List;
import java.util.Map;

public class TagbodyElement implements Element {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<LispStruct, List<LispStruct>> tagbodyForms;

	public TagbodyElement(final Map<LispStruct, List<LispStruct>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<LispStruct, List<LispStruct>> getTagbodyForms() {
		return tagbodyForms;
	}
}
