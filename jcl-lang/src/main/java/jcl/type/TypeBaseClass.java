/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.SymbolStruct;

/**
 * Abstract base class for all {@link LispType} implementations.
 */
public class TypeBaseClass extends SymbolStruct implements LispType {

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 */
	protected TypeBaseClass(final String name) {
		super(name, GlobalPackageStruct.JCL_TYPE);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		setValue(this);
	}

	@Override
	public String toString() {
		return getName();
	}
}