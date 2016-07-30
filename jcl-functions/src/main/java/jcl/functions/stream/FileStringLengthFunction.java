/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.math.BigInteger;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.array.StringStructImpl;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.StreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class FileStringLengthFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FILE-STRING-LENGTH";
	private static final String STREAM_ARGUMENT = "STREAM";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public FileStringLengthFunction() {
		super("Returns the difference between what (file-position stream) would be after writing object and its current value, or nil if this cannot be determined.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		final LispStruct lispStruct2 = arguments.getRequiredArgument(OBJECT_ARGUMENT);

		if (lispStruct2 instanceof CharacterStruct) {
			return IntegerStruct.ONE;
		} else if (lispStruct2 instanceof StringStructImpl) {
			final Long length = ((StringStructImpl) lispStruct2).length();
			return IntegerStruct.valueOf(BigInteger.valueOf(length));
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}
}
