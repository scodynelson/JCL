/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.math.BigDecimal;

import jcl.numbers.FloatStruct;
import jcl.printer.LispPrinter;
import jcl.reader.struct.ReaderVariables;
import jcl.types.DoubleFloat;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class FloatStructPrinter implements LispPrinter<FloatStruct> {

	private static final long serialVersionUID = 5762270611242790794L;

	@Override
	public String print(final FloatStruct object) {
		final jcl.types.Float floatFormat = (jcl.types.Float) object.getType();
		final jcl.types.Float defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getValue();

		final BigDecimal bigDecimal = object.getBigDecimal();
		String bigDecimalString = bigDecimal.toString();
		if (!floatFormat.equals(defaultFloatFormat)) {
			if (floatFormat.equals(ShortFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'S');
			} else if (floatFormat.equals(SingleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'F');
			} else if (floatFormat.equals(DoubleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'D');
			} else if (floatFormat.equals(LongFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'L');
			}
		}

		return bigDecimalString;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
