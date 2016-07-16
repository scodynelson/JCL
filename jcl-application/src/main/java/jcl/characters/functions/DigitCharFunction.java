/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code digit-char}.
 */
@Component
public final class DigitCharFunction extends CommonLispBuiltInFunctionStruct {

	/**
	 * Public constructor passing the documentation string.
	 */
	public DigitCharFunction() {
		super("If weight is less than radix, digit-char returns a character which has that weight when considered as a " +
				      "digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will " +
				      "be an uppercase character. If weight is greater than or equal to radix, digit-char returns false.",
		      "DIGIT-CHAR",
		      Parameters.forFunction("DIGIT-CHAR")
		                .requiredParameter("WEIGHT")
		                .optionalParameter("RADIX").withInitialValue(IntegerStruct.TEN)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the {@code digit-char} character function that expects an {@link IntegerStruct} weight
	 * parameter object with an optional {@link IntegerStruct} radix parameter object and applies {@link
	 * CharacterStruct#digitChar(IntegerStruct, IntegerStruct)} against the value and the radix value retrieved from
	 * the parameters passed to the {@link #getRadix(LispStruct...)} method to retrieve the weighted {@link
	 * CharacterStruct} for the {@link IntegerStruct} parameter weight and optional radix value.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the weighted {@link CharacterStruct} for the {@link IntegerStruct} parameter weight and optional radix
	 * value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct weight = arguments.getRequiredArgument("WEIGHT", IntegerStruct.class);
		final IntegerStruct radix = arguments.getRequiredArgument("RADIX", IntegerStruct.class);
		return CharacterStruct.digitChar(weight, radix);
	}
}
