/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.lists.NullStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.Collections;
import java.util.List;

public final class NullElement implements ListElement {

	public static final NullElement INSTANCE = new NullElement();

	private static final long serialVersionUID = -5896411725518882129L;

	private final NullStruct nullStruct;

	private NullElement() {
		nullStruct = NullStruct.INSTANCE;
	}

	public NullStruct getNullStruct() {
		return nullStruct;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	@Override
	public LispStruct toLispStruct() {
		return nullStruct;
	}

	@Override
	public List<SimpleElement> getElements() {
		return Collections.emptyList();
	}
}
