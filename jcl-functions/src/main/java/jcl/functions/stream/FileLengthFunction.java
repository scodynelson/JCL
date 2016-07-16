/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.StreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class FileLengthFunction extends CommonLispBuiltInFunctionStruct {

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
			return IntegerStruct.valueOf(BigInteger.valueOf(fileLength));
		}
	}
}
