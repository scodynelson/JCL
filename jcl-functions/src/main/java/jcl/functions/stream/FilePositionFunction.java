/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.math.BigInteger;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StreamStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class FilePositionFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FILE-POSITION";
	private static final String STREAM_ARGUMENT = "STREAM";
	private static final String POSITION_ARGUMENT = "POSITION";

	public FilePositionFunction() {
		super("Returns the length of stream, or nil if the length cannot be determined.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		                .optionalParameter(POSITION_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct stream = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);

		final LispStruct lispStruct2 = arguments.getOptionalArgument(POSITION_ARGUMENT);
//		validator.validateTypes(lispStruct2, functionName, "File Position", IntegerType.INSTANCE, SymbolType.INSTANCE);

		final Long position;
		if (lispStruct2 instanceof IntegerStruct) {
			position = ((IntegerStruct) lispStruct2).longValue();
		} else if (CommonLispSymbols.START_KEYWORD.equals(lispStruct2)) {
			position = 0L;
		} else if (CommonLispSymbols.END_KEYWORD.equals(lispStruct2)) {
			position = stream.fileLength();
		} else if (NILStruct.INSTANCE.equals(lispStruct2)) {
			position = null;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		if (position == null) {
			final Long currentPosition = stream.filePosition(null);
			return LispStructFactory.toInteger(BigInteger.valueOf(currentPosition));
		} else {
			final Long newPosition = stream.filePosition(position);
			return LispStructFactory.toBoolean(newPosition != null);
		}
	}
}
