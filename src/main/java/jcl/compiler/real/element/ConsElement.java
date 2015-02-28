/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ConsElement implements ListElement {

	private static final long serialVersionUID = -8355540372664272579L;

	private boolean isDotted;

	private EnhancedLinkedList<SimpleElement> elements;

	public ConsElement(final SimpleElement... elements) {
		this(new EnhancedLinkedList<>(Arrays.asList(elements)));
	}

	public ConsElement(final EnhancedLinkedList<SimpleElement> elements) {
		this(false, elements);
	}

	public ConsElement(final boolean isDotted, final EnhancedLinkedList<SimpleElement> elements) {
		this.elements = elements;
		this.isDotted = isDotted;
	}

	public ConsElement(final boolean isDotted, final SimpleElement... elements) {
		this(isDotted, new EnhancedLinkedList<>(Arrays.asList(elements)));
	}

	public boolean isDotted() {
		return isDotted;
	}

	@Override
	public EnhancedLinkedList<SimpleElement> getElements() {
		return elements;
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
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
