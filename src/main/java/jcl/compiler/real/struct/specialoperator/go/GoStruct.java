/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class GoStruct<T extends LispStruct> extends SpecialOperatorStruct {

	private static final long serialVersionUID = -4331758400526441262L;

	private final T tag;

	protected GoStruct(final T tag) {
		this.tag = tag;
	}

	public T getTag() {
		return tag;
	}
}
