/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code digit-char-p}.
 */
@Component
public final class DigitCharPFunction extends AbstractCharacterRadixFunction {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public DigitCharPFunction() {
		super("Tests whether character is a digit in the specified radix. If it is a digit in that radix, its weight is " +
				"returned as an integer; otherwise nil is returned.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} character object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} character object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final CharacterStruct character
				= validator.validateType(lispStructs[0], functionName(), "Character", CharacterType.INSTANCE, CharacterStruct.class);

		final IntegerStruct radix = getRadix(lispStructs);
		return character.charDigit(radix);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code digit-char-p} as a string.
	 *
	 * @return the function name {@code digit-char-p} as a string
	 */
	@Override
	protected String functionName() {
		return "DIGIT-CHAR-P";
	}
}
