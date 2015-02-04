/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

import java.util.List;
import java.util.Map;

public class TagbodyElement implements Element {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<Element, List<Element>> tagbodyForms;

	public TagbodyElement(final Map<Element, List<Element>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<Element, List<Element>> getTagbodyForms() {
		return tagbodyForms;
	}
}
