/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.Deque;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.streams.ConcatenatedStreamStruct;
import jcl.streams.InputStream;
import org.springframework.stereotype.Component;

@Component
public final class ConcatenatedStreamStreamsFunction extends CommonLispBuiltInFunctionStruct {

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
		final Deque<InputStream> inputStreams = concatenatedStream.getInputStreams();
		return ListStruct.buildProperList(new ArrayList<LispStruct>(inputStreams));
	}
}
