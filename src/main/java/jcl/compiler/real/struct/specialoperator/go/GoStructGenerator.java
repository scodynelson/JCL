/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.LispStruct;

public abstract class GoStructGenerator<T extends LispStruct> {

	protected GoStructGenerator() {
	}

	public abstract GoStruct<T> generateGoElement(final T tag);
}
