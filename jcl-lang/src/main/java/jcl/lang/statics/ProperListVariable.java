/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.NILStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class ProperListVariable extends VariableStructImpl<ListStruct> {

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