/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringInputStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-STRING-INPUT-STREAM";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String START_ARGUMENT = "START";
	private static final String END_ARGUMENT = "END";

	public MakeStringInputStreamFunction() {
		super("Returns an input string stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_ARGUMENT)
		                .optionalParameter(START_ARGUMENT).withInitialValue(IntegerStructImpl.ZERO)
		                .optionalParameter(END_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct aString = arguments.getRequiredArgument(STRING_ARGUMENT, StringStruct.class);
		final String javaString = aString.getAsJavaString();

		int start = 0;
		int end = javaString.length();

		if (arguments.hasOptionalArgument(START_ARGUMENT)) {
			final LispStruct startParam = arguments.getOptionalArgument(START_ARGUMENT);

			if (startParam instanceof IntegerStructImpl) {
				final int possibleStartValue = ((IntegerStructImpl) startParam).intValue();
				if (possibleStartValue > end) {
					throw new TypeErrorException(functionName + ": Start value must be less than or equal to the length of the String. Got: " + startParam);
				}
				start = possibleStartValue;
			}
		}

		if (arguments.hasOptionalArgument(END_ARGUMENT)) {
			final LispStruct endParam = arguments.getOptionalArgument(END_ARGUMENT);

			if (endParam instanceof IntegerStructImpl) {
				final int possibleEndValue = ((IntegerStructImpl) endParam).intValue();
				if (possibleEndValue > end) {
					throw new TypeErrorException(functionName + ": End value must be less than or equal to the length of the String. Got: " + endParam);
				}
				end = possibleEndValue;
			}
		}

		return LispStructFactory.toStringInputStream(javaString, start, end);
	}
}
