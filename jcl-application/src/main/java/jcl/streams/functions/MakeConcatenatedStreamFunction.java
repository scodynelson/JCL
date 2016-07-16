/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.ConcatenatedStreamStruct;
import jcl.streams.InputStream;
import org.springframework.stereotype.Component;

@Component
public final class MakeConcatenatedStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-CONCATENATED-STREAM";

	public MakeConcatenatedStreamFunction() {
		super("Returns a concatenated stream that has the indicated input-streams initially associated with it.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<InputStream> rest = arguments.getRestArgument(InputStream.class);
		final Deque<InputStream> inputStreams = new ArrayDeque<>(rest);
		return new ConcatenatedStreamStruct(inputStreams);
	}
}
