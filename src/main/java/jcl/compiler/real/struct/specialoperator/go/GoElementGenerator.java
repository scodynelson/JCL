/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.LispStruct;

public abstract class GoElementGenerator<T extends LispStruct> {

	protected GoElementGenerator() {
	}

	public abstract GoElement<T> generateGoElement(final T tag);
}
