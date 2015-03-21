/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.Printer;
import jcl.printer.LispPrinter;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import jcl.streams.TwoWayStreamStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TwoWayStreamStructPrinter implements LispPrinter<TwoWayStreamStruct> {

	private static final long serialVersionUID = -7785883762162329655L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final TwoWayStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final InputStream inputStream = object.getInputStream();
		final String printedInputStream = printer.print(inputStream);

		final OutputStream outputStream = object.getOutputStream();
		final String printedOutputStream = printer.print(outputStream);

		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
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
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
