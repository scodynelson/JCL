/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.numbers.RatioStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class RatioElement implements Element {

	private static final long serialVersionUID = 5051668665204048945L;

	private final RatioStruct ratioStruct;

	public RatioElement(final RatioStruct ratioStruct) {
		this.ratioStruct = ratioStruct;
	}

	public RatioStruct getRatioStruct() {
		return ratioStruct;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
