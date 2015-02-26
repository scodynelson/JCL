/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.numbers.FloatStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigDecimal;

public class FloatElement implements NumberElement {

	private static final long serialVersionUID = 9120922223192617896L;

	private final jcl.types.Float floatFormat;

	private final BigDecimal bigDecimal;

	public FloatElement(final jcl.types.Float floatFormat, final BigDecimal bigDecimal) {
		this.floatFormat = floatFormat;
		this.bigDecimal = bigDecimal;
	}

	public jcl.types.Float getFloatFormat() {
		return floatFormat;
	}

	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}

	@Override
	public LispStruct toLispStruct() {
		return new FloatStruct(floatFormat, bigDecimal);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
