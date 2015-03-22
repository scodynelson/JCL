/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.LispStruct;
import jcl.hashtables.HashTableStruct;
import jcl.printer.Printer;
import jcl.printer.LispPrinter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HashTableKeyWrapperPrinter implements LispPrinter<HashTableStruct.KeyWrapper> {

	private static final long serialVersionUID = 7702478572510338520L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final HashTableStruct.KeyWrapper object) {
		final LispStruct key = object.getKey();
		return printer.print(key);
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
		final HashTableKeyWrapperPrinter rhs = (HashTableKeyWrapperPrinter) obj;
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
