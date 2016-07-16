/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code digit-char-p}.
 */
@Component
public final class DigitCharPFunction extends CommonLispBuiltInFunctionStruct {

	/**
	 * Public constructor passing the documentation string.
	 */
	public DigitCharPFunction() {
		super("Tests whether character is a digit in the specified radix. If it is a digit in that radix, its weight is " +
				      "returned as an integer; otherwise nil is returned.",
		      "DIGIT-CHAR-P",
		      Parameters.forFunction("DIGIT-CHAR-P")
		                .requiredParameter("CHARACTER")
		                .optionalParameter("RADIX").withInitialValue(IntegerStruct.TEN)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the {@code digit-char-p} character function that expects an {@link CharacterStruct}
	 * parameter object with an optional {@link IntegerStruct} radix parameter object and applies {@link
	 * CharacterStruct#charDigit(IntegerStruct)} against the value and the radix value retrieved from the parameters
	 * passed to the {@link #getRadix(LispStruct...)} method to retrieve the weighted {@link IntegerStruct} character
	 * code for the {@link CharacterStruct} parameter and optional radix value.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the weighted {@link IntegerStruct} character code for the {@link CharacterStruct} parameter and optional
	 * radix value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument("CHARACTER", CharacterStruct.class);
		final IntegerStruct radix = arguments.getRequiredArgument("RADIX", IntegerStruct.class);
		return character.charDigit(radix);
	}
}
