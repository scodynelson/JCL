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
import jcl.streams.BroadcastStreamStruct;
import jcl.streams.OutputStream;
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
