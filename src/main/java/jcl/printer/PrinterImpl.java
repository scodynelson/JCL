/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class PrinterImpl implements Printer {

	private static final long serialVersionUID = -3051919400352866531L;

	@Resource
	private Map<Class<? extends LispStruct>, LispPrinter<LispStruct>> printerStrategies;

	@Override
	public String print(final LispStruct object) {

		final LispPrinter<LispStruct> printer = printerStrategies.get(object.getClass());
		if (printer == null) {
			final String typeClassName = object.getType().getClass().getName().toUpperCase();
			return "#<" + typeClassName + '>';
		}

		return printer.print(object);
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
