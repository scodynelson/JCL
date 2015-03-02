/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.lists.NullStruct;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public final class NullElement implements ListElement {

	public static final NullElement INSTANCE = new NullElement();

	private static final long serialVersionUID = -5896411725518882129L;

	private NullElement() {
	}

	@Override
	public EnhancedLinkedList<SimpleElement> getElements() {
		return new EnhancedLinkedList<>();
	}

	@Override
	public LispStruct toLispStruct() {
		return NullStruct.INSTANCE;
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
