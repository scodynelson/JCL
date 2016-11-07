/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.SystemBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.readtable.ReaderMacroFunction;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public abstract class ReaderMacroFunctionImpl extends SystemBuiltInFunctionStructBase implements ReaderMacroFunction {

	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String MACRO_CHARACTER_ARGUMENT = "MACRO-CHARACTER";
	private static final String N_ARGUMENT = "N";

	protected ReaderMacroFunctionImpl(final String functionName) {
		super("Some Documentation",
		      functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter(INPUT_STREAM_ARGUMENT)
		                .requiredParameter(MACRO_CHARACTER_ARGUMENT)
		                .optionalParameter(N_ARGUMENT)
		                .withInitialValue(IntegerStruct.ZERO)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStreamStruct stream = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT, InputStreamStruct.class);

		final CharacterStruct macroCharacter = arguments.getRequiredArgument(MACRO_CHARACTER_ARGUMENT, CharacterStruct.class);
		final int codePoint = macroCharacter.getCodePoint();

		Optional<BigInteger> numberArgument = Optional.empty();
		if (isDispatch() && arguments.hasOptionalArgument(N_ARGUMENT)) {
			final IntegerStruct macroNumberArgument = arguments.getOptionalArgument(N_ARGUMENT, IntegerStruct.class);
			final BigInteger bigInteger = macroNumberArgument.bigIntegerValue();
			numberArgument = Optional.of(bigInteger);
		}

		final ReaderInputStreamStruct readerInputStreamStruct = new ReaderInputStreamStruct(stream);
		return readMacro(readerInputStreamStruct, codePoint, numberArgument);
	}
}
