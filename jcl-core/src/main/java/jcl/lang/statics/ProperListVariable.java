/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
class ProperListVariable extends VariableStructImpl<ListStruct> {

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
			log.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			super.setValue(NILStruct.INSTANCE);
		}
	}
}
