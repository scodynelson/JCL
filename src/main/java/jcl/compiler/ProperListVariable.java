/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler;

import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.VariableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class ProperListVariable extends VariableStruct<ListStruct> {

	private static final long serialVersionUID = -4038470250934507806L;

	private static final Logger LOGGER = LoggerFactory.getLogger(ProperListVariable.class);

	ProperListVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, NullStruct.INSTANCE);
	}

	@Override
	public void setValue(final ListStruct value) {

		if (value.isProper()) {
			super.setValue(value);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, value);

			super.setValue(NullStruct.INSTANCE);
		}
	}
}