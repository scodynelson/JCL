/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.compiler.real.element.SimpleElement;

public abstract class GoElementGenerator<T extends SimpleElement> {

	protected GoElementGenerator() {
	}

	public abstract GoElement<T> generateGoElement(final T tag);
}
