/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class GoStruct<T extends LispStruct> extends CompilerSpecialOperatorStruct {

	private final T tag;

	protected GoStruct(final T tag) {
		this.tag = tag;
	}

	public T getTag() {
		return tag;
	}

	@Override
	public boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof GoStruct)
						&& tag.eq(((GoStruct<?>) object).tag));
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
