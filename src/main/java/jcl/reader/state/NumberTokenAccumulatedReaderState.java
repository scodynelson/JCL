/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.numbers.NumberStruct;
import jcl.reader.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
@Component
class NumberTokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8364722183939779239L;

	/**
	 * The list of {@link AttributeType}s that should not be present in a numeric token.
	 */
	private static final List<AttributeType> NOT_NUMBER_ATTRIBUTES = Arrays.asList(AttributeType.INVALID, AttributeType.ALPHABETIC, AttributeType.PACKAGEMARKER);

	/**
	 * The list of {@link AttributeType}s that there should not be more than one of if present in a numeric token.
	 */
	private static final List<AttributeType> NOT_MORE_THAN_ONE_ATTRIBUTES = Arrays.asList(AttributeType.DECIMAL, AttributeType.RATIOMARKER);

	/**
	 * {@link SymbolTokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private SymbolTokenAccumulatedReaderState symbolTokenAccumulatedReaderState;

	/**
	 * {@link FloatTokenAccumulatedReaderState} singleton used to accumulate a float token.
	 */
	@Autowired
	private FloatTokenAccumulatedReaderState floatTokenAccumulatedReaderState;

	/**
	 * {@link IntegerTokenAccumulatedReaderState} singleton used to accumulate an integer token.
	 */
	@Autowired
	private IntegerTokenAccumulatedReaderState integerTokenAccumulatedReaderState;

	/**
	 * {@link RationalFloatTokenAccumulatedReaderState} singleton used to accumulate a float token from a rational
	 * token.
	 */
	@Autowired
	private RationalFloatTokenAccumulatedReaderState rationalFloatTokenAccumulatedReaderState;

	/**
	 * {@link RationalTokenAccumulatedReaderState} singleton used to accumulate a rational token.
	 */
	@Autowired
	private RationalTokenAccumulatedReaderState rationalTokenAccumulatedReaderState;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		final NumberStruct numberToken = getNumberToken(tokenBuilder);
		if (numberToken == null) {
			return symbolTokenAccumulatedReaderState.process(tokenBuilder);
		} else {
			return numberToken;
		}
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
	private NumberStruct getNumberToken(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		// If there are no tokens, not a number. NOTE: We should never get here in the sequence. This is a protection.
		if (CollectionUtils.isEmpty(tokenAttributes)) {
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
		final boolean hasNoAlphaDigits = ReaderState.hasNoAttributesWithAttributeType(tokenAttributes, AttributeType.ALPHADIGIT);
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

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Strip off the ending '.' if it exists
		if (lastAttributeType == AttributeType.DECIMAL) {
			tokenAttributes.removeLast();
		}

		final boolean hasDecimal = ReaderState.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.DECIMAL);
		final boolean hasExponentMarker = ReaderState.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.EXPONENTMARKER);
		final boolean hasRatioMarker = ReaderState.hasAnyAttributeWithAttributeType(tokenAttributes, AttributeType.RATIOMARKER);

		if (hasDecimal && hasRatioMarker) {
			return rationalFloatTokenAccumulatedReaderState.process(tokenBuilder);
		}

		if (hasDecimal || hasExponentMarker) {
			return floatTokenAccumulatedReaderState.process(tokenBuilder);
		}

		if (hasRatioMarker) {
			return rationalTokenAccumulatedReaderState.process(tokenBuilder);
		}

		return integerTokenAccumulatedReaderState.process(tokenBuilder);
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
		final int currentRadix = ReaderVariables.READ_BASE.getValue().getBigInteger().intValueExact();

		final int digit = Character.digit(currentToken, currentRadix);
		final boolean isDigitWithRadix = digit >= 0;

		final Character.UnicodeBlock firstTokenCodePointBlock = Character.UnicodeBlock.of(firstTokenCodePoint);
		final boolean isDigitInSameBlock = Character.UnicodeBlock.of(currentToken).equals(firstTokenCodePointBlock);
		return isDigitWithRadix && isDigitInSameBlock;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
