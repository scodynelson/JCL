/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class GoStruct<T extends LispStruct> extends CompilerSpecialOperatorStruct {

	private final T tag;

	protected GoStruct(final T tag) {
		this.tag = tag;
	}

	public T getTag() {
		return tag;
	}
}
