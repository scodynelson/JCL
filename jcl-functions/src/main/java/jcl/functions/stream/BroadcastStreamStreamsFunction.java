/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayList;
import java.util.Deque;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.BroadcastStreamStructImpl;
import jcl.lang.stream.OutputStreamStruct;
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
		final BroadcastStreamStructImpl broadcastStream = arguments.getRequiredArgument(BROADCAST_STREAM_ARGUMENT, BroadcastStreamStructImpl.class);
		final Deque<OutputStreamStruct> outputStreamStructs = broadcastStream.getOutputStreamStructs();
		return LispStructFactory.toProperList(new ArrayList<LispStruct>(outputStreamStructs));
	}
}
