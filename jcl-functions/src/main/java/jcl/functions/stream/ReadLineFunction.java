/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.StreamVariables;
import jcl.lang.stream.ReadLineResult;
import org.springframework.stereotype.Component;

@Component
public final class ReadLineFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "READ-LINE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

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

		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		final ReadLineResult readLineResult = inputStreamStruct.readLine(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		final String result = readLineResult.getResult();
		final boolean eof = readLineResult.isEof();
		return ValuesStruct.valueOf(LispStructFactory.toString(result), LispStructFactory.toBoolean(eof));
	}
}
