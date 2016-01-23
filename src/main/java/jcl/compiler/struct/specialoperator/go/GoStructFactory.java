/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.LispStruct;

public abstract class GoStructFactory<T extends LispStruct> {

	protected GoStructFactory() {
	}

	public abstract GoStruct<T> getGoElement(final T tag);
}
