/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.InputStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeConcatenatedStreamFunction extends CommonLispBuiltInFunctionStructBase {

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
		final List<InputStreamStruct> rest = arguments.getRestArgument(InputStreamStruct.class);
		final Deque<InputStreamStruct> inputStreamStructs = new ArrayDeque<>(rest);
		return LispStructFactory.toConcatenatedStream(inputStreamStructs);
	}
}
