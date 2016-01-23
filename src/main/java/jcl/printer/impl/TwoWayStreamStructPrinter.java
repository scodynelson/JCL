/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.LispPrinter;
import jcl.printer.Printer;
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

	@Autowired
	private Printer printer;

	@Override
	public String print(final TwoWayStreamStruct object) {
		final String typeClassName = object.getType().getClass().getSimpleName().toUpperCase();

		final InputStream inputStream = object.getInputStream();
		final String printedInputStream = printer.print(inputStream);

		final OutputStream outputStream = object.getOutputStream();
		final String printedOutputStream = printer.print(outputStream);

		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final TwoWayStreamStructPrinter rhs = (TwoWayStreamStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
