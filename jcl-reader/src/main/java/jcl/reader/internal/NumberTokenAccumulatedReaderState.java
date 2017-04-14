/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RatioStruct;
import jcl.lang.RationalStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.AttributeType;
import jcl.lang.statics.ReaderVariables;
import jcl.type.DoubleFloatType;
import jcl.type.FloatType;
import jcl.type.LongFloatType;
import jcl.type.ShortFloatType;
import jcl.type.SingleFloatType;
import jcl.util.CodePointConstants;
import jcl.util.NumberUtils;

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
final class NumberTokenAccumulatedReaderState {

	/**
	 * The list of {@link AttributeType}s that should not be present in a numeric token.
	 */
	private static final List<AttributeType> NOT_NUMBER_ATTRIBUTES = Arrays.asList(AttributeType.INVALID, AttributeType.ALPHABETIC, AttributeType.PACKAGEMARKER);

	/**
	 * The list of {@link AttributeType}s that there should not be more than one of if present in a numeric token.
	 */
	private static final List<AttributeType> NOT_MORE_THAN_ONE_ATTRIBUTES = Arrays.asList(AttributeType.DECIMAL, AttributeType.RATIOMARKER);

	/**
	 * Private constructor.
	 */
	private NumberTokenAccumulatedReaderState() {
	}

	/**
	 * This method gets a {@link NumberStruct} from the provided {@link TokenBuilder} and it's {@link
	 * TokenBuilder#tokenAttributes}.
	 *
	 * @param tokenBuilder
	 * 		the reader state containing the {@link TokenBuilder#tokenAttributes} to derive the {@link NumberStruct}
	 *
	 * @return the built {@link NumberStruct} value
	 */
	static NumberStruct getNumberToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// If there are no tokens, not a number. NOTE: We should never get here in the sequence. This is a protection.
		if (tokenAttributes.isEmpty()) {
			return null;
		}

		// If there are any 'INVALID', 'ALPHABETIC', or 'PACKAGEMARKER' tokens, not a number
		final boolean containsNonNumberAttrs
				= tokenAttributes.stream()
				                 .map(TokenAttribute::getAttributeType)
				                 .anyMatch(NOT_NUMBER_ATTRIBUTES::contains);
		if (containsNonNumberAttrs) {
			return null;
		}

		// Check that there is at least 1 'ALPHADIGIT'
		final boolean hasNoAlphaDigits = ReaderProcessor.hasNoAttributesWithAttributeType(tokenAttributes, AttributeType.ALPHADIGIT);
		if (hasNoAlphaDigits) {
			return null;
		}

		// Check all 'ALPHADIGIT' tokens to make sure they are digits within the current radix
		// Unicode (not in original spec)
		final boolean areAnyTokensInvalidRegexAndUnicode = !areValidNumericTokens(tokenAttributes);
		if (areAnyTokensInvalidRegexAndUnicode) {
			return null;
		}

		// TODO: I would LOVE to figure out how to combine the following 2 stream operations to just produce the final boolean.
		// TODO: Haven't learned the streams facility well enough yet to figure it out...

		// Checks to make sure there are not more than one of: 'DECIMAL', 'RATIOMARKER'
		final Map<AttributeType, Long> attributeTypeToAttributes
				= tokenAttributes.stream()
				                 .map(TokenAttribute::getAttributeType)
				                 .filter(NOT_MORE_THAN_ONE_ATTRIBUTES::contains)
				                 .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));

		final Collection<Long> attributeTypeEntries = attributeTypeToAttributes.values();
		final boolean hasMoreThanOneOfAttributes
				= attributeTypeEntries.stream()
				                      .anyMatch(e -> e > 1);

		if (hasMoreThanOneOfAttributes) {
			return null;
		}

		// If first token is an exponent marker, this is a symbol not a number
		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();
		if (firstAttributeType == AttributeType.EXPONENTMARKER) {
			return null;
		}

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Strip off the ending '.' if it exists
		if (lastAttributeType == AttributeType.DECIMAL) {
			tokenAttributes.removeLast();
		}

		final boolean hasDecimal = ReaderProcessor.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.DECIMAL);
		final boolean hasExponentMarker = ReaderProcessor.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.EXPONENTMARKER);
		final boolean hasRatioMarker = ReaderProcessor.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.RATIOMARKER);

		if (hasDecimal && hasRatioMarker) {
			return processRationalFloat(tokenBuilder);
		}

		if (hasDecimal || hasExponentMarker) {
			return processFloatToken(tokenBuilder);
		}

		if (hasRatioMarker) {
			return processRationalToken(tokenBuilder);
		}

		return processIntegerToken(tokenBuilder);
	}

	/**
	 * Determines if the provided {@code tokenAttributes} are valid parts of a numeric token, using the {@link
	 * ReaderVariables#READ_BASE} value and ensuring they are in the same Unicode block as the provided first token
	 * attribute.
	 *
	 * @param tokenAttributes
	 * 		the current token attributes to verify are valid parts of a numeric token
	 *
	 * @return true if the provided {@code tokenAttributes} are valid parts of a numeric token; false otherwise
	 */
	private static boolean areValidNumericTokens(final LinkedList<TokenAttribute> tokenAttributes) {
		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final int firstTokenCodePoint = firstTokenAttribute.getCodePoint();

		return tokenAttributes.stream()
		                      .filter(e -> e.getAttributeType() == AttributeType.ALPHADIGIT)
		                      .mapToInt(TokenAttribute::getCodePoint)
		                      .mapToObj(e -> isValidNumericToken(firstTokenCodePoint, e))
		                      .allMatch(Boolean.TRUE::equals);
	}

	/**
	 * Determines if provided {@code currentToken} is a valid part of a numeric token, using the {@link
	 * ReaderVariables#READ_BASE} value and ensuring it is in the same Unicode block as the provided {@code
	 * firstToken}.
	 *
	 * @param firstTokenCodePoint
	 * 		the first token code point in the item to process
	 * @param currentToken
	 * 		the current token to verify is a valid part of a numeric token
	 *
	 * @return true if the provided {@code currentToken} is a valid part of a numeric token; false otherwise
	 */
	private static boolean isValidNumericToken(final int firstTokenCodePoint, final int currentToken) {
		final int currentRadix = ReaderVariables.READ_BASE.getVariableValue().intValue();

		final int digit = Character.digit(currentToken, currentRadix);
		final boolean isDigitWithRadix = digit >= 0;

		final Character.UnicodeBlock firstTokenCodePointBlock = Character.UnicodeBlock.of(firstTokenCodePoint);
		final boolean isDigitInSameBlock = Character.UnicodeBlock.of(currentToken).equals(firstTokenCodePointBlock);
		return isDigitWithRadix && isDigitInSameBlock;
	}

	/*
		Integer
	 */

	/**
	 * The list of {@link AttributeType}s that should only be first if present in a numeric token.
	 */
	private static final List<AttributeType> FIRST_ONLY_ATTRIBUTES = Arrays.asList(AttributeType.PLUS, AttributeType.MINUS);

	/**
	 * Sub-piece of Reader algorithm part 10.1, used to produce an {@link IntegerStruct} output when a rational token is
	 * supplied with no {@link AttributeType#RATIOMARKER} nor {@link AttributeType#DECIMAL}.
	 */
	private static NumberStruct processIntegerToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final List<TokenAttribute> allButFirstTokenAttribute = tokenAttributes.subList(1, tokenAttributes.size());

		// Checks to make sure if either 'PLUS' or 'MINUS' is supplied, that it is first
		final boolean hasAttributesAndNotFirst
				= allButFirstTokenAttribute.stream()
				                           .map(TokenAttribute::getAttributeType)
				                           .anyMatch(FIRST_ONLY_ATTRIBUTES::contains);

		if (hasAttributesAndNotFirst) {
			return null;
		}

		final String tokenString = ReaderProcessor.convertTokenAttributesToString(tokenAttributes);
		final int currentRadix = ReaderVariables.READ_BASE.getVariableValue().intValue();

		final BigInteger bigInteger = new BigInteger(tokenString, currentRadix);
		return LispStructFactory.toInteger(bigInteger);
	}

	/*
		Float
	 */

	/**
	 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a float token is
	 * supplied. This means using the correct exponential {@link RoundingMode#HALF_UP} to produce an accurate float result.
	 */
	private static NumberStruct processFloatToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final Integer exponentTokenCodePoint = ReaderProcessor.getTokenCodePointByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		String tokenString = ReaderProcessor.convertTokenAttributesToString(tokenAttributes);
		tokenString = getFloatTokenString(tokenString, exponentTokenCodePoint);

		// TODO: FloatType???
		final FloatType floatType = getFloatType(exponentTokenCodePoint);
		return LispStructFactory.toFloat(tokenString);

//		if (DoubleFloatType.INSTANCE.equals(floatType) || LongFloatType.INSTANCE.equals(floatType)) {
//			try {
//				final Double d = Double.parseDouble(tokenString);
//				return DoubleFloatStruct.valueOf(d);
//			} catch (final NumberFormatException ignored) {
//				return null;
//			}
//		} else {
//			try {
//				final Float f = Float.parseFloat(tokenString);
//				return SingleFloatStruct.valueOf(f);
//			} catch (final NumberFormatException ignored) {
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
	private static String getFloatTokenString(final String tokenString, final Integer exponentTokenCodePoint) {
		if (exponentTokenCodePoint != null) {
			final String exponentTokenString = String.valueOf(Character.toChars(exponentTokenCodePoint));
			final String eCapitalLetterString = CodePointConstants.LATIN_CAPITAL_LETTER_E.toString();
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
	private static FloatType getFloatType(final Integer exponentTokenCodePoint) {
		FloatType floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		if (exponentTokenCodePoint != null) {
			final int exponentTokenInt = exponentTokenCodePoint;
			if ((exponentTokenInt == CodePointConstants.LATIN_SMALL_LETTER_S) || (exponentTokenInt == CodePointConstants.LATIN_CAPITAL_LETTER_S)) {
				floatType = ShortFloatType.INSTANCE;
			} else if ((exponentTokenInt == CodePointConstants.LATIN_SMALL_LETTER_F) || (exponentTokenInt == CodePointConstants.LATIN_CAPITAL_LETTER_F)) {
				floatType = SingleFloatType.INSTANCE;
			} else if ((exponentTokenInt == CodePointConstants.LATIN_SMALL_LETTER_D) || (exponentTokenInt == CodePointConstants.LATIN_CAPITAL_LETTER_D)) {
				floatType = DoubleFloatType.INSTANCE;
			} else if ((exponentTokenInt == CodePointConstants.LATIN_SMALL_LETTER_L) || (exponentTokenInt == CodePointConstants.LATIN_CAPITAL_LETTER_L)) {
				floatType = LongFloatType.INSTANCE;
			} else if ((exponentTokenInt == CodePointConstants.LATIN_SMALL_LETTER_E) || (exponentTokenInt == CodePointConstants.LATIN_CAPITAL_LETTER_E)) {
				floatType = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();
			}
		}
		return floatType;
	}

	/*
		Rational
	 */

	/**
	 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link RatioStruct} output when a rational token is
	 * supplied with an {@link AttributeType#RATIOMARKER}. This will also produce an {@link IntegerStruct} when the
	 * resulting {@link BigInteger} has a denominator of {@link BigInteger#ONE}.
	 */
	private static NumberStruct processRationalToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Checks to make sure if either 'RATIOMARKER' is supplied, that it is neither first nor last
		if ((firstAttributeType == AttributeType.RATIOMARKER) || (lastAttributeType == AttributeType.RATIOMARKER)) {
			return null;
		}

		final String tokenString = ReaderProcessor.convertTokenAttributesToString(tokenAttributes);

		final int numberOfRationalParts = 2;
		final String[] rationalParts = tokenString.split("/", numberOfRationalParts);

		final int currentRadix = ReaderVariables.READ_BASE.getVariableValue().intValue();

		final BigInteger numerator = new BigInteger(rationalParts[0], currentRadix);
		final BigInteger denominator = new BigInteger(rationalParts[1], currentRadix);
		return RationalStruct.valueOf(numerator, denominator);
	}

	/*
		Rational Float
	 */

	/**
	 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a rational token is
	 * supplied with both an {@link AttributeType#RATIOMARKER} and {@link AttributeType#DECIMAL}. This means using the
	 * correct exponential division using {@link MathContext#DECIMAL128} and {@link RoundingMode#HALF_UP} to produce an
	 * accurate float result.
	 */
	private static NumberStruct processRationalFloat(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Checks to make sure if either 'RATIOMARKER' is supplied, that it is neither first nor last
		if ((firstAttributeType == AttributeType.RATIOMARKER) || (lastAttributeType == AttributeType.RATIOMARKER)) {
			return null;
		}

		final String tokenString = ReaderProcessor.convertTokenAttributesToString(tokenAttributes);

		final int numberOfRationalParts = 2;
		final String[] rationalParts = tokenString.split("/", numberOfRationalParts);

		final String numeratorPart = rationalParts[0];

		// Numerator cannot contain a DECIMAL
		if (numeratorPart.contains(".")) {
			return null;
		}

		final Integer exponentTokenCodePoint = ReaderProcessor.getTokenCodePointByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		final String numeratorTokenString = getFloatTokenString(numeratorPart, exponentTokenCodePoint);
		final BigDecimal numeratorBigDecimal = NumberUtils.bigDecimalValue(numeratorTokenString);

		final String denominatorTokenString = getFloatTokenString(rationalParts[1], exponentTokenCodePoint);
		final BigDecimal denominatorBigDecimal;
		try {
			denominatorBigDecimal = NumberUtils.bigDecimalValue(denominatorTokenString);
		} catch (final NumberFormatException ignored) {
			// NOTE: we don't check the 'numeratorBigDecimal' because it MUST be an integer token, therefore we won't
			//       have the issues with the BigDecimal creations
			return null;
		}

		final BigDecimal bigDecimal = numeratorBigDecimal.divide(denominatorBigDecimal, MathContext.DECIMAL128);

		// TODO: Not sure this is the best way to handle rational floats for the read algorithm. Might be a better way.
		// TODO: FloatType???
		final FloatType floatType = getFloatType(exponentTokenCodePoint);
		return LispStructFactory.toFloat(bigDecimal);

//		if (DoubleFloatType.INSTANCE.equals(floatType) || LongFloatType.INSTANCE.equals(floatType)) {
//			try {
//				final Double d = bigDecimal.doubleValue();
//				return DoubleFloatStruct.valueOf(d);
//			} catch (final NumberFormatException ignored) {
//				return null;
//			}
//		} else {
//			try {
//				final Float f = bigDecimal.floatValue();
//				return SingleFloatStruct.valueOf(f);
//			} catch (final NumberFormatException ignored) {
//				return null;
//			}
//		}
	}
}
