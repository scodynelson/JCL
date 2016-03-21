/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.NILStruct;
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
			return new IntegerStruct(BigInteger.valueOf(fileLength));
		}
	}
}
