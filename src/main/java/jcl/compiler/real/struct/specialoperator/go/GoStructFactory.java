/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import java.io.Serializable;

import jcl.LispStruct;

public abstract class GoStructFactory<T extends LispStruct> implements Serializable {

	private static final long serialVersionUID = -5463110914990614222L;

	protected GoStructFactory() {
	}

	public abstract GoStruct<T> getGoElement(final T tag);
}
