/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayList;
import java.util.Deque;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.ConcatenatedStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ConcatenatedStreamStreamsFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "CONCATENATED-STREAM-STREAMS";
	private static final String CONCATENATED_STREAM_ARGUMENT = "CONCATENATED-STREAM";

	public ConcatenatedStreamStreamsFunction() {
		super("Returns a list of input streams that constitute the ordered set of streams the concatenated-stream still has to read from, starting with the current one it is reading from.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CONCATENATED_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ConcatenatedStreamStruct concatenatedStream = arguments.getRequiredArgument(CONCATENATED_STREAM_ARGUMENT, ConcatenatedStreamStruct.class);
		final Deque<InputStreamStruct> inputStreamStructs = concatenatedStream.getInputStreamStructs();
		return LispStructFactory.toProperList(new ArrayList<LispStruct>(inputStreamStructs));
	}
}
