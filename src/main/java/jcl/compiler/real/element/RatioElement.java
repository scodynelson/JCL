/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.numbers.RatioStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

public class RatioElement implements RationalElement {

	private static final long serialVersionUID = 5051668665204048945L;

	private final BigFraction bigFraction;

	public RatioElement(final BigFraction bigFraction) {
		this.bigFraction = bigFraction;
	}

	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public LispStruct toLispStruct() {
		return new RatioStruct(bigFraction);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
