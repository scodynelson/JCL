/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.ReaderVariables;
import org.springframework.stereotype.Component;

@Component
public final class CopyReadtableFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COPY-READTABLE";
	private static final String FROM_READTABLE_ARGUMENT = "FROM-READTABLE";
	private static final String TO_READTABLE_ARGUMENT = "TO-READTABLE";

	public CopyReadtableFunction() {
		super("Copies from-readtable.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(FROM_READTABLE_ARGUMENT).withInitialValue(ReaderVariables.READTABLE.getVariableValue())
		                .optionalParameter(TO_READTABLE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ReadtableStruct fromReadtable = arguments.getOptionalArgument(FROM_READTABLE_ARGUMENT, ReadtableStruct.class);
		final LispStruct toReadtable = arguments.getOptionalArgument(TO_READTABLE_ARGUMENT);

		if (toReadtable instanceof ReadtableStruct) {
			// TODO: This isn't correct. We can't fully copy the Readtable objects right now.
			final ReadtableStruct toReadtableCast = (ReadtableStruct) toReadtable;
			return LispStructFactory.toReadtable(toReadtableCast.getReadtableCase());
		} else {
			return LispStructFactory.toReadtable();
		}
	}
}
