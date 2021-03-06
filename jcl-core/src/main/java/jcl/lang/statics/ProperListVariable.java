/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import lombok.extern.log4j.Log4j2;

@Log4j2
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

		final LispStruct last = variableValue.last(IntegerStruct.ZERO);
		if (NILStruct.INSTANCE.eq(last)) {
			super.setValue(variableValue);
		} else {
			log.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			super.setValue(NILStruct.INSTANCE);
		}
	}
}
