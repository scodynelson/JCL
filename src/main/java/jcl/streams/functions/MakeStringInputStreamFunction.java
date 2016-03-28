/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.printer.Printer;
import jcl.streams.StringInputStreamStruct;
import jcl.symbols.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringInputStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-STRING-INPUT-STREAM";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String START_ARGUMENT = "START";
	private static final String END_ARGUMENT = "END";

	@Autowired
	private Printer printer;

	public MakeStringInputStreamFunction() {
		super("Returns an input string stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_ARGUMENT)
		                .optionalParameter(START_ARGUMENT).withInitialValue(IntegerStruct.ZERO)
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

			if (startParam instanceof IntegerStruct) {
				final int possibleStartValue = ((IntegerStruct) startParam).intValue();
				if (possibleStartValue > end) {
					final String printedObject = printer.print(startParam);
					throw new TypeErrorException(functionName + ": Start value must be less than or equal to the length of the String. Got: " + printedObject);
				}
				start = possibleStartValue;
			}
		}

		if (arguments.hasOptionalArgument(END_ARGUMENT)) {
			final LispStruct endParam = arguments.getOptionalArgument(END_ARGUMENT);

			if (endParam instanceof IntegerStruct) {
				final int possibleEndValue = ((IntegerStruct) endParam).intValue();
				if (possibleEndValue > end) {
					final String printedObject = printer.print(endParam);
					throw new TypeErrorException(functionName + ": End value must be less than or equal to the length of the String. Got: " + printedObject);
				}
				end = possibleEndValue;
			}
		}

		return new StringInputStreamStruct(javaString, start, end);
	}
}
