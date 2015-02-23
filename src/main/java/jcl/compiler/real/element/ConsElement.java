/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ConsElement implements ListElement {

	private static final long serialVersionUID = -8355540372664272579L;

	private List<SimpleElement> elements;

	private boolean isDotted;

	public ConsElement(final SimpleElement... elements) {
		this(Arrays.asList(elements));
	}

	public ConsElement(final List<SimpleElement> elements) {
		this(false, elements);
	}

	public ConsElement(final boolean isDotted, final List<SimpleElement> elements) {
		this.elements = elements;
		this.isDotted = isDotted;
	}

	public ConsElement(final boolean isDotted, final SimpleElement... elements) {
		this(isDotted, Arrays.asList(elements));
	}

	@Override
	public List<SimpleElement> getElements() {
		return elements;
	}

	public boolean isDotted() {
		return isDotted;
	}

	@Override
	public LispStruct toLispStruct() {
		final List<LispStruct> lispStructs = elements.stream()
		                                             .map(SimpleElement::toLispStruct)
		                                             .collect(Collectors.toList());

		if (isDotted) {
			return ListStruct.buildDottedList(lispStructs);
		} else {
			return ListStruct.buildProperList(lispStructs);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
