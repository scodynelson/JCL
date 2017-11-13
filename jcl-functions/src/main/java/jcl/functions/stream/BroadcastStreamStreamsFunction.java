/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayList;
import java.util.Deque;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BroadcastStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class BroadcastStreamStreamsFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "BROADCAST-STREAM-STREAMS";
	private static final String BROADCAST_STREAM_ARGUMENT = "BROADCAST-STREAM";

	public BroadcastStreamStreamsFunction() {
		super("Returns a list of output streams that constitute all the streams to which the broadcast-stream is broadcasting.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(BROADCAST_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final BroadcastStreamStruct broadcastStream = arguments.getRequiredArgument(BROADCAST_STREAM_ARGUMENT, BroadcastStreamStruct.class);
		final Deque<OutputStreamStruct> outputStreamStructs = broadcastStream.getOutputStreamStructs();
		return ListStruct.toLispList(new ArrayList<LispStruct>(outputStreamStructs));
	}
}
