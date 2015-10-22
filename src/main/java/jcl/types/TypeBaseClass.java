/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.LispType;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

/**
 * Abstract base class for all {@link LispType} implementations.
 */
public class TypeBaseClass extends SymbolStruct<LispType> implements LispType {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3499497199319905092L;

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
