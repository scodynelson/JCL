/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.PeekType;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PeekCharFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PEEK-CHAR";
	private static final String PEEK_TYPE_ARGUMENT = "PEEK-TYPE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	@Autowired
	private Printer printer;

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
		if (TStruct.INSTANCE.equals(lispStruct1)) {
			peekType = PeekType.T_PEEK_TYPE;
		} else if (NILStruct.INSTANCE.equals(lispStruct1)) {
			peekType = PeekType.NIL_PEEK_TYPE;
		} else if (lispStruct1 instanceof CharacterStruct) {
			final CharacterStruct character = (CharacterStruct) lispStruct1;
			peekType = PeekType.getCharacterPeekType(character.getCodePoint());
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		final LispStruct lispStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT);
		final InputStream inputStream;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStream) {
			inputStream = (InputStream) lispStruct;
		} else {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		final ReadPeekResult readPeekResult = inputStream.peekChar(peekType, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.valueOf(readPeekResult.getResult());
	}
}
