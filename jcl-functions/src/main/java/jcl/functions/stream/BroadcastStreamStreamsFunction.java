/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayList;
import java.util.Deque;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.stream.BroadcastStreamStruct;
import jcl.lang.stream.OutputStream;
import org.springframework.stereotype.Component;

@Component
public final class BroadcastStreamStreamsFunction extends CommonLispBuiltInFunctionStruct {

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
		final Deque<OutputStream> outputStreams = broadcastStream.getOutputStreams();
		return ListStruct.buildProperList(new ArrayList<LispStruct>(outputStreams));
	}
}
