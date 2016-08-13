/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.FileStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class StreamExternalFormatFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "STREAM-EXTERNAL-FORMAT";
	private static final String FILE_STREAM_ARGUMENT = "FILE-STREAM";

	public StreamExternalFormatFunction() {
		super("Returns an external file format designator for the stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FILE_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FileStreamStruct fileStream = arguments.getRequiredArgument(FILE_STREAM_ARGUMENT, FileStreamStruct.class);
		return fileStream.getExternalFormat();
	}
}
