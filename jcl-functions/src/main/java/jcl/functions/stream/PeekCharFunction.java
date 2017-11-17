/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.StreamVariables;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.stereotype.Component;

@Component
public final class PeekCharFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "PEEK-CHAR";
	private static final String PEEK_TYPE_ARGUMENT = "PEEK-TYPE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	public PeekCharFunction() {
		super("Obtains the next character in input-stream without actually reading it.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(PEEK_TYPE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct lispStruct1 = arguments.getOptionalArgument(PEEK_TYPE_ARGUMENT);
		final PeekType peekType;
		if (TStruct.INSTANCE.eq(lispStruct1)) {
			peekType = PeekType.T_PEEK_TYPE;
		} else if (NILStruct.INSTANCE.eq(lispStruct1)) {
			peekType = PeekType.NIL_PEEK_TYPE;
		} else if (lispStruct1 instanceof CharacterStruct) {
			final CharacterStruct character = (CharacterStruct) lispStruct1;
			peekType = PeekType.getCharacterPeekType(character.toUnicodeCodePoint());
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		final LispStruct lispStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT);
		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.eq(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.eq(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		final ReadPeekResult readPeekResult = inputStreamStruct.peekChar(peekType, eofErrorP.toJavaPBoolean(), eofValue, recursiveP.toJavaPBoolean());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.toLispCharacter(readPeekResult.getResult());
	}
}
