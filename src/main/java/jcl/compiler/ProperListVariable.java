/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.VariableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class ProperListVariable extends VariableStruct<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProperListVariable.class);

	ProperListVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, NILStruct.INSTANCE);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof ListStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be List value.");
		}
		final ListStruct variableValue = (ListStruct) value;

		if (variableValue.isProper()) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			super.setValue(NILStruct.INSTANCE);
		}
	}
}
