/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code digit-char}.
 */
@Component
public final class DigitCharFunction extends CommonLispBuiltInFunctionStructBase {

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
	 * the parameters passed in to retrieve the weighted {@link
	 * CharacterStruct} for the {@link IntegerStruct} parameter weight and optional radix value.
	 *
	 * @param arguments
	 * 		the function parameters
	 *
	 * @return the weighted {@link CharacterStruct} for the {@link IntegerStruct} parameter weight and optional radix
	 * value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct weight = arguments.getRequiredArgument("WEIGHT", IntegerStruct.class);
		if (arguments.hasOptionalArgument("RADIX")) {
			final IntegerStruct radix = arguments.getOptionalArgument("RADIX", IntegerStruct.class);
			return CharacterStruct.digitChar(weight, radix);

		}
		return CharacterStruct.digitChar(weight);
	}
}
