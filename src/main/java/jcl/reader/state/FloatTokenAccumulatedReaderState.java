/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.characters.CharacterConstants;
import jcl.numbers.FloatStruct;
import jcl.numbers.NumberStruct;
import jcl.reader.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import jcl.types.DoubleFloat;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.LinkedList;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a float token is
 * supplied. This means using the correct exponential {@link RoundingMode#HALF_UP} to produce an accurate float result.
 */
@Component
public class FloatTokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3908401531730468150L;

	@Override
	public NumberStruct process(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final Integer exponentToken = ReaderState.getTokenByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		String tokenString = ReaderState.convertTokensToString(tokenAttributes);
		tokenString = getFloatTokenString(tokenString, exponentToken);

		BigDecimal bigDecimal = new BigDecimal(tokenString);

		final int scale = bigDecimal.scale();
		if (scale < 1) {
			bigDecimal = bigDecimal.setScale(1, RoundingMode.HALF_UP);
		}

		final jcl.types.Float aFloat = getFloatType(exponentToken);
		return new FloatStruct(aFloat, bigDecimal);
	}

	/**
	 * Gets the float token string from the provided tokenString and exponentToken. The exponentToken determines how the
	 * tokenString exponent should be replaced to look like a valid exponent string in Java.
	 *
	 * @param tokenString
	 * 		the tokenString
	 * @param exponentToken
	 * 		the exponentToken
	 *
	 * @return the proper float token string
	 */
	static String getFloatTokenString(final String tokenString, final Integer exponentToken) {
		if (exponentToken != null) {
			final String exponentTokenString = String.valueOf(Character.toChars(exponentToken));
			final String eCapitalLetterString = CharacterConstants.LATIN_CAPITAL_LETTER_E.toString();
			return tokenString.replace(exponentTokenString, eCapitalLetterString);
		}
		return tokenString;
	}

	/**
	 * Gets the float type from the based off of the exponentToken parameter.
	 *
	 * @param exponentToken
	 * 		the exponentToken used to determine the float type
	 *
	 * @return the proper float type
	 */
	static jcl.types.Float getFloatType(final Integer exponentToken) {
		jcl.types.Float floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getValue();

		if (exponentToken != null) {
			final int exponentTokenInt = exponentToken;
			if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_S) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_S)) {
				floatType = ShortFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_F) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_F)) {
				floatType = SingleFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_D) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_D)) {
				floatType = DoubleFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_L) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_L)) {
				floatType = LongFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_E) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_E)) {
				floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getValue();
			}
		}
		return floatType;
	}
}
