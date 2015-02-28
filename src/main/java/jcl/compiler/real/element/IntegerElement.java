/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.numbers.IntegerStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigInteger;

public class IntegerElement implements RationalElement {

	private static final long serialVersionUID = 4314662992381073176L;

	private final BigInteger bigInteger;

	public IntegerElement(final BigInteger bigInteger) {
		this.bigInteger = bigInteger;
	}

	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public LispStruct toLispStruct() {
		return new IntegerStruct(bigInteger);
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
