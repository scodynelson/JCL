/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StreamStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class FileLengthFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FILE-LENGTH";
	private static final String STREAM_ARGUMENT = "STREAM";

	public FileLengthFunction() {
		super("Returns the length of stream, or nil if the length cannot be determined.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct streamStruct = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		final Long fileLength = streamStruct.fileLength();
		if (fileLength == null) {
			return NILStruct.INSTANCE;
		} else {
			return IntegerStruct.toLispInteger(fileLength);
		}
	}
}
