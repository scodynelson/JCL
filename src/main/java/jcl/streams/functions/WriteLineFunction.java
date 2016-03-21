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
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class WriteLineFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "WRITE-LINE";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	@Autowired
	private Printer printer;

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

		final OutputStream outputStream;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (lispStruct instanceof OutputStream) {
			outputStream = (OutputStream) lispStruct;
		} else {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Output Stream.");
		}

		final IntegerStruct startParam = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
		final int start = startParam.getBigInteger().intValue();

		final LispStruct endParam = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD);

		final String javaString = stringParam.getAsJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).getBigInteger().intValue();
			outputStream.writeLine(javaString, start, end);
		} else {
			outputStream.writeLine(javaString, start);
		}

		return startParam;
	}
}
