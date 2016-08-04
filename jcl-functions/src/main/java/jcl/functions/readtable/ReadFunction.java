/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.lang.BooleanStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.InputStreamStruct;
import jcl.lang.statics.StreamVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReadFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "READ";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	@Autowired
	private ApplicationContext context;

	public ReadFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT)
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct inputStreamArg = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT);
		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.equals(inputStreamArg)) {
			inputStreamStruct = StreamVariables.TERMINAL_IO.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(inputStreamArg)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (inputStreamArg instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) inputStreamArg;
		} else {
			throw new TypeErrorException("The value " + inputStreamArg + " is not either T, NIL, or a STREAM.");
		}

		final BooleanStructImpl eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStructImpl.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStructImpl recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStructImpl.class);
		return read(inputStreamStruct, eofErrorP, eofValue, recursiveP);
	}

	public LispStruct read(final InputStreamStruct inputStreamStruct, final BooleanStructImpl eofErrorP, final LispStruct eofValue,
	                       final BooleanStructImpl recursiveP) {

		return read(inputStreamStruct, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public LispStruct read(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStreamStruct);
		return reader.read(eofErrorP, eofValue, recursiveP);
	}

	public LispStruct read(final Reader reader, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		return reader.read(eofErrorP, eofValue, recursiveP);
	}
}
