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
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code digit-char}.
 */
@Component
public final class DigitCharFunction extends AbstractCharacterRadixFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8526559424608179479L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public DigitCharFunction() {
		super("If weight is less than radix, digit-char returns a character which has that weight when considered as a " +
				"digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will " +
				"be an uppercase character. If weight is greater than or equal to radix, digit-char returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} integer object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} integer object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "WEIGHT").buildList();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final IntegerStruct weight
				= validator.validateType(lispStructs[0], functionName(), "Weight", IntegerType.INSTANCE, IntegerStruct.class);

		final IntegerStruct radix = getRadix(lispStructs);
		return CharacterStruct.digitChar(weight, radix);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code digit-char} as a string.
	 *
	 * @return the function name {@code digit-char} as a string
	 */
	@Override
	protected String functionName() {
		return "DIGIT-CHAR";
	}
}
