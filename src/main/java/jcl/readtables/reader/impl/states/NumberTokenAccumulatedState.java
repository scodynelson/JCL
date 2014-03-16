package jcl.readtables.reader.impl.states;

import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.readtables.reader.StateReader;
import jcl.readtables.reader.impl.TokenBuilder;
import jcl.syntax.AttributeType;
import jcl.syntax.CharacterConstants;
import jcl.syntax.reader.TokenAttribute;
import jcl.types.DoubleFloat;
import jcl.types.Float;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;
import jcl.variables.ReadBaseVariable;
import jcl.variables.ReadDefaultFloatFormatVariable;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.LinkedList;

/**
 * Step 10.1 of the Reader Algorithm.
 * <p/>
 * This state is reached when we have accumulated a token, and it needs to be processed into a
 * 1) Number/PotentialNumber
 * <p/>
 * First we check to see if the token is a number, if it is, then we attempt to format it.  If it cannot
 * be formatted, then we progress to the SymbolTokenAccumulatedState.
 * <p/>
 */
public class NumberTokenAccumulatedState extends State {

	public static final State NUMBER_TOKEN_ACCUMULATED_STATE = new NumberTokenAccumulatedState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return SymbolTokenAccumulatedState    if a number token could not be created
	 * EndState                       the final accepting state
	 */
	@Override
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {

		final NumberStruct numberToken = getNumberToken(tokenBuilder);
		if (numberToken == null) {
			SymbolTokenAccumulatedState.SYMBOL_TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		} else {
			tokenBuilder.setReturnToken(numberToken);
		}
	}

	/**
	 * This method gets a numberToken from the provided tokenBuilder and it's tokenAttributes.
	 *
	 * @param tokenBuilder the reader state containing the tokenAttributes to derive the numberToken
	 * @return the built numberToken value
	 */
	private static NumberStruct getNumberToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// If there are no tokens, not a number
		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return null;
		}

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoAlphaDigits = StateUtils.hasNoAttribute(tokenAttributes, AttributeType.ALPHADIGIT);
		if (hasNoAlphaDigits) {
			return null;
		}

		// If there are any 'INVALID', 'ALPHABETIC', or 'PACKAGEMARKER' tokens, not a number
		for (final TokenAttribute tokenAttribute : tokenAttributes) {
			final AttributeType attributeType = tokenAttribute.getAttributeType();
			if ((attributeType == AttributeType.INVALID)
					|| (attributeType == AttributeType.ALPHABETIC)
					|| (attributeType == AttributeType.PACKAGEMARKER)) {
				return null;
			}
		}

		// Check number attributes
		final boolean areNumberAttributesInvalid = areNumberAttributesInvalid(tokenAttributes);
		if (areNumberAttributesInvalid) {
			return null;
		}

		// Check all 'ALPHADIGIT' tokens to make sure they are digits within the current radix
		// Unicode (not in original spec)
		//  Check to make sure all digits are from the same Unicode block
		final int currentRadix = ReadBaseVariable.INSTANCE.getValue();
		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final int firstToken = firstTokenAttribute.getToken();
		final Character.UnicodeBlock block = Character.UnicodeBlock.of(firstToken);

		final boolean areAnyTokensInvalidRegexAndUnicode = StateUtils.areAnyTokensInvalidRegexAndUnicode(currentRadix, block, tokenAttributes);
		if (areAnyTokensInvalidRegexAndUnicode) {
			return null;
		}

		String tokenString = StateUtils.convertTokensToString(tokenAttributes);

		// Java does not support numbers in the format +12345, so we need to get rid of the plus sign if it exists
		if (StringUtils.startsWith(tokenString, "+")) {
			tokenString = StringUtils.substring(tokenString, 1);
		}

		// Strip off the ending '.' if it exists
		if (StringUtils.endsWith(tokenString, ".")) {
			tokenString = StringUtils.substring(tokenString, 0, tokenString.length() - 1);
		}

		final boolean hasDecimal = StateUtils.hasAttribute(tokenAttributes, AttributeType.DECIMAL);
		final boolean hasRatioMarker = StateUtils.hasAttribute(tokenAttributes, AttributeType.RATIOMARKER);
		final boolean hasExponentMarker = StateUtils.hasAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		if (hasExponentMarker && !hasDecimal) {
			return null;
		} else if (hasRatioMarker) {
			final String[] rationalParts = StringUtils.split(tokenString, "/", 2);
			final BigInteger numerator = new BigInteger(rationalParts[0], currentRadix);
			final BigInteger denominator = new BigInteger(rationalParts[1], currentRadix);

			final BigFraction rational = new BigFraction(numerator, denominator);
			return new RatioStruct(rational);
		} else if (hasDecimal) {
			final Integer exponentToken = StateUtils.getTokenByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

			tokenString = getFloatTokenString(tokenString, exponentToken);

			final Float aFloat = getFloatType(exponentToken);
			final BigDecimal bigDecimal = new BigDecimal(tokenString);
			return new FloatStruct(aFloat, bigDecimal);
		} else {
			final BigInteger basicInteger = new BigInteger(tokenString, currentRadix);
			return new IntegerStruct(basicInteger);
		}
	}

	/**
	 * This method gets the float type from the based off of the exponentToken parameter.
	 *
	 * @param exponentToken the exponentToken used to determine the float type
	 * @return the proper float type
	 */
	private static Float getFloatType(final Integer exponentToken) {
		if (exponentToken != null) {
			final int exponentTokenInt = exponentToken;
			if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_S) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_S)) {
				return ShortFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_F) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_F)) {
				return SingleFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_D) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_D)) {
				return DoubleFloat.INSTANCE;
			} else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_L) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_L)) {
				return LongFloat.INSTANCE;
			}
//			else if ((exponentTokenInt == CharacterConstants.LATIN_SMALL_LETTER_E) || (exponentTokenInt == CharacterConstants.LATIN_CAPITAL_LETTER_E)) {
//				return ReadDefaultFloatFormatVariable.INSTANCE.getValue();
//			}
		}
		return ReadDefaultFloatFormatVariable.INSTANCE.getValue();
	}

	/**
	 * This method gets the float token string from the provided tokenString and exponentToken. The exponentToken determines
	 * how the tokenString exponent should be replaced to look like a valid exponent string in Java.
	 *
	 * @param tokenString   the tokenString
	 * @param exponentToken the exponentToken
	 * @return the proper float token string
	 */
	private static String getFloatTokenString(final String tokenString, final Integer exponentToken) {
		if (exponentToken != null) {
			final String exponentTokenString = String.valueOf(exponentToken);
			final String eCapitalLetterString = CharacterConstants.LATIN_CAPITAL_LETTER_E.toString();
			return StringUtils.replace(tokenString, exponentTokenString, eCapitalLetterString);
		}
		return tokenString;
	}

	/**
	 * This method checks to see if any of the number specific attributes are invalidly placed in the token, which would
	 * make the token non-numeric.
	 *
	 * @param tokenAttributes the token attributes to check
	 * @return true if any of the tokens are invalidly placed
	 * false if all the tokens are validly placed
	 */
	private static boolean areNumberAttributesInvalid(final LinkedList<TokenAttribute> tokenAttributes) {

		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Checks to make sure there are not more than one of: 'PLUS', 'MINUS', 'DECIMAL', 'RATIOMARKER'
		final boolean hasMoreThanOneOfAttributes = StateUtils.hasMoreThanOneOfAttributes(tokenAttributes,
				AttributeType.PLUS, AttributeType.MINUS, AttributeType.DECIMAL, AttributeType.RATIOMARKER);

		// Checks to make sure if either 'PLUS' or 'MINUS' is supplied, that it is first
		final boolean hasAttributesAndNotFirst = StateUtils.hasAttributesAndNotFirst(firstAttributeType, tokenAttributes,
				AttributeType.PLUS, AttributeType.MINUS);

		// Checks to make sure if either 'DECIMAL' or 'RATIOMARKER' is supplied, that it is neither first nor last
		final boolean hasAttributesAndFirstOrLast = StateUtils.hasAttributesAndFirstOrLast(firstAttributeType, lastAttributeType, tokenAttributes,
				AttributeType.DECIMAL, AttributeType.RATIOMARKER);

		// Checks to make sure that both 'DECIMAL' and 'RATIOMARKER' are not supplied at the same time
		final boolean hasAttributes = StateUtils.hasAttributes(tokenAttributes, AttributeType.DECIMAL, AttributeType.RATIOMARKER);

		return hasMoreThanOneOfAttributes || hasAttributesAndNotFirst || hasAttributesAndFirstOrLast || hasAttributes;
	}
}
