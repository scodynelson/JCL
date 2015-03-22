/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.math.BigInteger;

import jcl.functions.FunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HashTableStructPrinter implements LispPrinter<HashTableStruct> {

	private static final long serialVersionUID = 8729748390924216544L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final HashTableStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final FunctionStruct test = object.getTest();
		final String printedTest = printer.print(test);

		final BigInteger mapSize = object.getCount();

		return "#<" + typeClassName + " :TEST " + printedTest + " size " + mapSize + '>';
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
		final HashTableStructPrinter rhs = (HashTableStructPrinter) obj;
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
