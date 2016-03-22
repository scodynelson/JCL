/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class FilePositionFunction extends CommonLispBuiltInFunctionStruct {

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
			position = ((IntegerStruct) lispStruct2).getBigInteger().longValue();
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
			return IntegerStruct.valueOf(BigInteger.valueOf(currentPosition));
		} else {
			final Long newPosition = stream.filePosition(position);
			return BooleanStructs.toLispBoolean(newPosition != null);
		}
	}
}
