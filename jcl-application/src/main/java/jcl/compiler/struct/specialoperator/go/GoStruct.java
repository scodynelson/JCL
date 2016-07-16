/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class GoStruct<T extends LispStruct> extends CompilerSpecialOperatorStruct {

	private final T tag;

	protected GoStruct(final T tag) {
		this.tag = tag;
	}

	public T getTag() {
		return tag;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if ((obj == null) || (getClass() != obj.getClass())) {
			return false;
		}

		final GoStruct<?> goStruct = (GoStruct<?>) obj;

		return new EqualsBuilder()
				.appendSuper(super.equals(obj))
				.append(tag, goStruct.tag)
				.isEquals();
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.appendSuper(super.hashCode())
				.append(tag)
				.toHashCode();
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(GO ");

		final String printedTag = tag.toString();
		builder.append(printedTag);

		builder.append(')');

		return builder.toString();
	}
}
