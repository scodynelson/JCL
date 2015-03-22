/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.streams.EchoStreamStruct;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EchoStreamStructPrinter implements LispPrinter<EchoStreamStruct> {

	private static final long serialVersionUID = 5872440805729953534L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final EchoStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final InputStream inputStream = object.getInputStream();
		final String printedInputStream = printer.print(inputStream);

		final OutputStream outputStream = object.getOutputStream();
		final String printedOutputStream = printer.print(outputStream);

		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(printer)
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
		final EchoStreamStructPrinter rhs = (EchoStreamStructPrinter) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
