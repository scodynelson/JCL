/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.OutputStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeBroadcastStreamFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MAKE-BROADCAST-STREAM";

	public MakeBroadcastStreamFunction() {
		super("Returns a broadcast stream that has the indicated input-streams initially associated with it.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<OutputStreamStruct> rest = arguments.getRestArgument(OutputStreamStruct.class);
		final Deque<OutputStreamStruct> outputStreamStructs = new ArrayDeque<>(rest);
		return LispStructFactory.toBroadcastStream(outputStreamStructs);
	}
}
