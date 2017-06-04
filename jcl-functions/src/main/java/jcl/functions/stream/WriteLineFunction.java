/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class WriteLineFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "WRITE-LINE";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public WriteLineFunction() {
		super("Writes the characters of the sub-sequence of string bounded by start and end to output-stream followed by a newline.",
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
		if (TStruct.INSTANCE.eq(lispStruct)) {
			outputStreamStruct = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.eq(lispStruct)) {
			outputStreamStruct = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (lispStruct instanceof OutputStreamStruct) {
			outputStreamStruct = (OutputStreamStruct) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Output Stream.");
		}

		final IntegerStruct startParam = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
		final int start = startParam.toJavaInt();

		final LispStruct endParam = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD);

		final String javaString = stringParam.toJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).toJavaInt();
			outputStreamStruct.writeLine(javaString, start, end);
		} else {
			outputStreamStruct.writeLine(javaString, start);
		}

		return startParam;
	}
}
