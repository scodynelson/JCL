/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.OutputStreamStruct;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class WriteStringFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "WRITE-STRING";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public WriteStringFunction() {
		super("Writes the characters of the sub-sequence of string bounded by start and end to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_ARGUMENT)
		                .optionalParameter(OUTPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		                .keyParameter(CommonLispSymbols.START_KEYWORD).withInitialValue(IntegerStruct.ZERO)
		                .keyParameter(CommonLispSymbols.END_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct stringParam = arguments.getRequiredArgument(STRING_ARGUMENT, StringStruct.class);
		final LispStruct lispStruct = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT);

		final OutputStreamStruct outputStreamStruct;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			outputStreamStruct = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			outputStreamStruct = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (lispStruct instanceof OutputStreamStruct) {
			outputStreamStruct = (OutputStreamStruct) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Output Stream.");
		}

		final IntegerStruct startParam = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
		final int start = startParam.intValue();

		final LispStruct endParam = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD);

		final String javaString = stringParam.getAsJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).intValue();
			outputStreamStruct.writeString(javaString, start, end);
		} else {
			outputStreamStruct.writeString(javaString, start);
		}

		return startParam;
	}
}
