/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.readtable.Reader;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.StreamVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReadPreservingWhitespaceFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "READ-PRESERVING-WHITESPACE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	@Autowired
	private ApplicationContext context;

	public ReadPreservingWhitespaceFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object preserving any whitespace character that delimits the printed representation of the object.",
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
		final InputStream inputStream;
		if (TStruct.INSTANCE.equals(inputStreamArg)) {
			inputStream = StreamVariables.TERMINAL_IO.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(inputStreamArg)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (inputStreamArg instanceof InputStream) {
			inputStream = (InputStream) inputStreamArg;
		} else {
			throw new TypeErrorException("The value " + inputStreamArg + " is not either T, NIL, or a STREAM.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);
		return readPreservingWhitespace(inputStream, eofErrorP, eofValue, recursiveP);
	}

	public LispStruct readPreservingWhitespace(final InputStream inputStream, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                                           final BooleanStruct recursiveP) {

		return readPreservingWhitespace(inputStream, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public LispStruct readPreservingWhitespace(final InputStream inputStream, final boolean eofErrorP, final LispStruct eofValue,
	                                           final boolean recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStream);
		return reader.readPreservingWhitespace(eofErrorP, eofValue, recursiveP);
	}

	public LispStruct readPreservingWhitespace(final Reader reader, final boolean eofErrorP, final LispStruct eofValue,
	                                           final boolean recursiveP) {

		return reader.readPreservingWhitespace(eofErrorP, eofValue, recursiveP);
	}
}