/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import lombok.extern.log4j.Log4j2;

@Log4j2
class ProperListVariable extends VariableStructImpl<ListStruct> {

	ProperListVariable(final String name) {
		super(name);
	}

	@Override
	public LispStruct setSymbolValue(final LispStruct value) {
		if (!(value instanceof ListStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be List value.");
		}
		final ListStruct variableValue = (ListStruct) value;

		final LispStruct last = variableValue.last(IntegerStruct.ZERO);
		if (NILStruct.INSTANCE.eq(last)) {
			return super.setSymbolValue(variableValue);
		} else {
			log.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			return super.setSymbolValue(NILStruct.INSTANCE);
		}
	}
}
