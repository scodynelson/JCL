/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.NumberStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.structs.symbols.variables.Variable;
import jcl.types.DoubleFloat;
import jcl.types.Float;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

/**
 * Step 10.1 of the Reader Algorithm.
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into a
 * 1) Number/PotentialNumber
 * </p>
 * <p>
 * First we check to see if the token is a number, if it is, then we attempt to format it.  If it cannot
 * be formatted, then we progress to the SymbolTokenAccumulatedState.
 * </p>
 */
final class NumberTokenAccumulatedState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new NumberTokenAccumulatedState();

	/**
	 * The list of {@link AttributeType}s that should not be present in a numeric token.
	 */
	private static final List<AttributeType> NOT_NUMBER_ATTRS = Arrays.asList(AttributeType.INVALID, AttributeType.ALPHABETIC, AttributeType.PACKAGEMARKER);

	/**
	 * The list of {@link AttributeType}s that there should not be more than one of if present in a numeric token.
	 */
	private static final List<AttributeType> NOT_MORE_THAN_ONE_ATTRS = Arrays.asList(AttributeType.PLUS, AttributeType.MINUS, AttributeType.DECIMAL, AttributeType.RATIOMARKER);

	/**
	 * The list of {@link AttributeType}s that should only be first if present in a numeric token.
	 */
	private static final List<AttributeType> FIRST_ONLY_ATTRS = Arrays.asList(AttributeType.PLUS, AttributeType.MINUS);

	/**
	 * The list of {@link AttributeType}s that should not be first nor last if present in a numeric token.
	 */
	private static final List<AttributeType> NOT_FIRST_OR_LAST_ATTRS = Arrays.asList(AttributeType.DECIMAL, AttributeType.RATIOMARKER);

	/**
	 * The list of {@link AttributeType}s that there should only be one of if present in a numeric token.
	 */
	private static final List<AttributeType> NO_SIMULTANEOUS_ATTRS = Arrays.asList(AttributeType.DECIMAL, AttributeType.RATIOMARKER);

	/**
	 * Private constructor.
	 */
	private NumberTokenAccumulatedState() {
	}

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final NumberStruct numberToken = getNumberToken(tokenBuilder);
		if (numberToken == null) {
			SymbolTokenAccumulatedState.INSTANCE.process(reader, tokenBuilder);
		} else {
			tokenBuilder.setReturnToken(numberToken);
		}
	}

	/**
	 * This method gets a numberToken from the provided tokenBuilder and it's tokenAttributes.
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the tokenAttributes to derive the numberToken
	 *
	 * @return the built numberToken value
	 */
	private static NumberStruct getNumberToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// If there are no tokens, not a number
		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return null;
		}

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoAlphaDigits = State.hasNoAttributes(tokenAttributes, AttributeType.ALPHADIGIT);
		if (hasNoAlphaDigits) {
			return null;
		}

		// If there are any 'INVALID', 'ALPHABETIC', or 'PACKAGEMARKER' tokens, not a number
		final boolean containsNonNumberAttrs =
				tokenAttributes.stream()
				               .anyMatch(NOT_NUMBER_ATTRS::contains);
		if (containsNonNumberAttrs) {
			return null;
		}

		// Check number attributes
		final boolean areNumberAttributesInvalid = areNumberAttributesInvalid(tokenAttributes);
		if (areNumberAttributesInvalid) {
			return null;
		}

		// Check all 'ALPHADIGIT' tokens to make sure they are digits within the current radix
		// Unicode (not in original spec)
		//  Check to make sure all digits are from the same Unicode block
		final int currentRadix = Variable.READ_BASE.getValue().getBigInteger().intValueExact();
		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final int firstToken = firstTokenAttribute.getToken();

		final boolean areAnyTokensInvalidRegexAndUnicode = areValidNumericTokens(currentRadix, firstToken, tokenAttributes);
		if (areAnyTokensInvalidRegexAndUnicode) {
			return null;
		}

		String tokenString = State.convertTokensToString(tokenAttributes);

		// Java does not support numbers in the format +12345, so we need to get rid of the plus sign if it exists
		if (tokenString.startsWith("+")) {
			tokenString = tokenString.substring(1);
		}

		// Strip off the ending '.' if it exists
		if (tokenString.endsWith(".")) {
			tokenString = tokenString.substring(0, tokenString.length() - 1);
		}

		final boolean hasDecimal = State.hasAnyAttribute(tokenAttributes, AttributeType.DECIMAL);
		final boolean hasRatioMarker = State.hasAnyAttribute(tokenAttributes, AttributeType.RATIOMARKER);
		final boolean hasExponentMarker = State.hasAnyAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		if (hasExponentMarker && !hasDecimal) {
			return null;
		} else if (hasRatioMarker) {
			final String[] rationalParts = tokenString.split("/", 2);
			final BigInteger numerator = new BigInteger(rationalParts[0], currentRadix);
			final BigInteger denominator = new BigInteger(rationalParts[1], currentRadix);

			final BigFraction rational = new BigFraction(numerator, denominator);
			return new RatioStruct(rational);
		} else if (hasDecimal) {
			final Integer exponentToken = State.getTokenByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

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
	 * This method checks to see if any of the number specific attributes are invalidly placed in the token, which
	 * would
	 * make the token non-numeric.
	 *
	 * @param tokenAttributes
	 * 		the token attributes to check
	 *
	 * @return true if any of the tokens are invalidly placed
	 * false if all the tokens are validly placed
	 */
	private static boolean areNumberAttributesInvalid(final LinkedList<TokenAttribute> tokenAttributes) {

		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Checks to make sure there are not more than one of: 'PLUS', 'MINUS', 'DECIMAL', 'RATIOMARKER'
		Function<AttributeType, Boolean> function =
				e -> {
					final long numberOfMatchingAttributes =
							tokenAttributes
									.stream()
									.filter(tokenAttribute -> tokenAttribute.getAttributeType() == e)
									.count();
					return numberOfMatchingAttributes > 1;
				};
		final boolean hasMoreThanOneOfAttributes
				= hasAttributes(function, NOT_MORE_THAN_ONE_ATTRS);

		// Checks to make sure if either 'PLUS' or 'MINUS' is supplied, that it is first
		function =
				e -> State.hasAnyAttribute(tokenAttributes, e) && (firstAttributeType != e);
		final boolean hasAttributesAndNotFirst
				= hasAttributes(function, FIRST_ONLY_ATTRS);

		// Checks to make sure if either 'DECIMAL' or 'RATIOMARKER' is supplied, that it is neither first nor last
		function =
				e -> State.hasAnyAttribute(tokenAttributes, e) && ((firstAttributeType == e) || (lastAttributeType == e));
		final boolean hasAttributesAndFirstOrLast
				= hasAttributes(function, NOT_FIRST_OR_LAST_ATTRS);

		// Checks to make sure that both 'DECIMAL' and 'RATIOMARKER' are not supplied at the same time
		final boolean hasAttributes =
				NO_SIMULTANEOUS_ATTRS
						.stream()
						.map(e -> State.hasAnyAttribute(tokenAttributes, e))
						.reduce(true, (result, e) -> result && e);

		return hasMoreThanOneOfAttributes || hasAttributesAndNotFirst || hasAttributesAndFirstOrLast || hasAttributes;
	}

	/**
	 * Determines if any of the provided {@code attributeTypes} are present according to the results of the application
	 * of the provided {@code function}.
	 *
	 * @param function
	 * 		the function to apply to determine the presence of the provided {@code attributeTypes}
	 * @param attributeTypes
	 * 		the AttributeType values to test for presence
	 *
	 * @return true if any of the provided {@code attributeTypes} are present; false otherwise
	 */
	private static boolean hasAttributes(final Function<AttributeType, Boolean> function, final List<AttributeType> attributeTypes) {
		return attributeTypes
				.stream()
				.map(function)
				.reduce(false, (result, e) -> result || e);
	}

	/**
	 * Determines if the provided {@code tokenAttributes} are valid parts of a numeric token, using the provided {@code
	 * currentRadix} and ensuring they are in the same Unicode block as the provided {@code firstToken}.
	 *
	 * @param currentRadix
	 * 		the current radix to parse the numeric value in
	 * @param firstToken
	 * 		the first token in the item to process
	 * @param tokenAttributes
	 * 		the current token attributes to verify are valid parts of a numeric token
	 *
	 * @return true if the provided {@code tokenAttributes} are valid parts of a numeric token; false otherwise
	 */
	private static boolean areValidNumericTokens(final int currentRadix, final int firstToken, final List<TokenAttribute> tokenAttributes) {
		return tokenAttributes
				.stream()
				.map(e -> (e.getAttributeType() != AttributeType.ALPHADIGIT) || isValidNumericToken(currentRadix, firstToken, e.getToken()))
				.reduce(false, (result, e) -> result || e);
	}

	/**
	 * Determines if provided {@code currentToken} is a valid part of a numeric token, using the provided {@code
	 * currentRadix} and ensuring it is in the same Unicode block as the provided {@code firstToken}.
	 *
	 * @param currentRadix
	 * 		the current radix to parse the numeric value in
	 * @param firstToken
	 * 		the first token in the item to process
	 * @param currentToken
	 * 		the current token to verify is a valid part of a numeric token
	 *
	 * @return true if the provided {@code currentToken} is a valid part of a numeric token; false otherwise
	 */
	private static boolean isValidNumericToken(final int currentRadix, final int firstToken, final int currentToken) {
		final boolean isDigitWithRadix = Character.digit(currentToken, currentRadix) >= 0;

		final Character.UnicodeBlock tokenBlock = Character.UnicodeBlock.of(firstToken);
		final boolean isDigitInSameBlock = Character.UnicodeBlock.of(currentToken).equals(tokenBlock);
		return !(isDigitWithRadix && isDigitInSameBlock);
	}

	/**
	 * This method gets the float token string from the provided tokenString and exponentToken. The exponentToken
	 * determines
	 * how the tokenString exponent should be replaced to look like a valid exponent string in Java.
	 *
	 * @param tokenString
	 * 		the tokenString
	 * @param exponentToken
	 * 		the exponentToken
	 *
	 * @return the proper float token string
	 */
	private static String getFloatTokenString(final String tokenString, final Integer exponentToken) {
		if (exponentToken != null) {
			final String exponentTokenString = String.valueOf(exponentToken);
			final String eCapitalLetterString = CharacterConstants.LATIN_CAPITAL_LETTER_E.toString();
			return tokenString.replace(exponentTokenString, eCapitalLetterString);
		}
		return tokenString;
	}

	/**
	 * This method gets the float type from the based off of the exponentToken parameter.
	 *
	 * @param exponentToken
	 * 		the exponentToken used to determine the float type
	 *
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
//				return Variable.READ_DEFAULT_FLOAT_FORMAT.getValue();
//			}
		}
		return Variable.READ_DEFAULT_FLOAT_FORMAT.getValue();
	}
}
