/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.math.RoundingMode;
import java.util.LinkedList;

import jcl.lang.CharacterConstants;
import jcl.lang.number.FloatStruct;
import jcl.lang.number.NumberStruct;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.ReaderVariables;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.type.DoubleFloatType;
import jcl.type.FloatType;
import jcl.type.LongFloatType;
import jcl.type.ShortFloatType;
import jcl.type.SingleFloatType;
import org.springframework.stereotype.Component;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a float token is
 * supplied. This means using the correct exponential {@link RoundingMode#HALF_UP} to produce an accurate float result.
 */
@Component
public class FloatTokenAccumulatedReaderState implements ReaderState {

	@Override
	public NumberStruct process(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final Integer exponentTokenCodePoint = ReaderState.getTokenCodePointByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		String tokenString = ReaderState.convertTokenAttributesToString(tokenAttributes);
		tokenString = getFloatTokenString(tokenString, exponentTokenCodePoint);

		// TODO: FloatType???
		final FloatType floatType = getFloatType(exponentTokenCodePoint);
		return FloatStruct.valueOf(tokenString);

//		if (DoubleFloatType.INSTANCE.equals(floatType) || LongFloatType.INSTANCE.equals(floatType)) {
//			try {
//				final Double d = Double.parseDouble(tokenString);
//				return DoubleFloatStruct.valueOf(d);
//			} catch (final NumberFormatException ignore) {
//				return null;
//			}
//		} else {
//			try {
//				final Float f = Float.parseFloat(tokenString);
//				return SingleFloatStruct.valueOf(f);
//			} catch (final NumberFormatException ignore) {
//				return null;
//			}
//		}
	}

	/**
	 * Gets the float token string from the provided tokenString and exponentToken code point. The exponentToken code
	 * point determines how the tokenString exponent should be replaced to look like a valid exponent string in Java.
	 *
	 * @param tokenString
	 * 		the tokenString
	 * @param exponentTokenCodePoint
	 * 		the exponentToken code point
	 *
	 * @return the proper float token string
	 */
	static String getFloatTokenString(final String tokenString, final Integer exponentTokenCodePoint) {
		if (exponentTokenCodePoint != null) {
			final String exponentTokenString = String.valueOf(Character.toChars(exponentTokenCodePoint));
			final String eCapitalLetterString = CharacterConstants.LATIN_CAPITAL_LETTER_E.toString();
			return tokenString.replace(exponentTokenString, eCapitalLetterString);
		}
		return tokenString;
	}

	/**
	 * Gets the float type from the based off of the exponentToken code point parameter.
	 *
	 * @param exponentTokenCodePoint
	 * 		the exponentToken code point used to determine the float type
	 *
	 * @return the proper float type
	 */
	static FloatType getFloatType(final Integer exponentTokenCodePoint) {
		FloatType floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		if (exponentTokenCodePoint != null) {
			final int exponentTokenInt = exponentTokenCodePoint;
			if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_S) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_S)) {
				floatType = ShortFloatType.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_F) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_F)) {
				floatType = SingleFloatType.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_D) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_D)) {
				floatType = DoubleFloatType.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_L) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_L)) {
				floatType = LongFloatType.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_E) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_E)) {
				floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();
			}
		}
		return floatType;
	}
}
