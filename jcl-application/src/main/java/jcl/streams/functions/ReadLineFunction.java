/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.lang.BooleanStruct;
import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.ReadLineResult;
import jcl.lang.stream.StreamVariables;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReadLineFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "READ-LINE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	@Autowired
	private Printer printer;

	public ReadLineFunction() {
		super("Returns the next line of text that is terminated by a newline or end of file.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT);

		final InputStream inputStream;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStream) {
			inputStream = (InputStream) lispStruct;
		} else {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		final ReadLineResult readLineResult = inputStream.readLine(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		final String result = readLineResult.getResult();
		final boolean eof = readLineResult.isEof();
		return new ValuesStruct(new StringStruct(result), BooleanStructs.toLispBoolean(eof));
	}
}
