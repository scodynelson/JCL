/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.numbers.FloatStruct;
import jcl.numbers.NumberStruct;
import jcl.reader.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.LinkedList;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a rational token is
 * supplied with both an {@link AttributeType#RATIOMARKER} and {@link AttributeType#DECIMAL}. This means using the
 * correct exponential division using {@link MathContext#DECIMAL128} and {@link RoundingMode#HALF_UP} to produce an
 * accurate float result.
 */
@Component
public class RationalFloatTokenAccumulatedReaderState extends FloatTokenAccumulatedReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3280863763820402148L;

	@Override
	public NumberStruct process(final TokenBuilder tokenBuilder) {

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final TokenAttribute firstTokenAttribute = tokenAttributes.getFirst();
		final AttributeType firstAttributeType = firstTokenAttribute.getAttributeType();

		final TokenAttribute lastTokenAttribute = tokenAttributes.getLast();
		final AttributeType lastAttributeType = lastTokenAttribute.getAttributeType();

		// Checks to make sure if either 'RATIOMARKER' is supplied, that it is neither first nor last
		if ((firstAttributeType == AttributeType.RATIOMARKER) || (lastAttributeType == AttributeType.RATIOMARKER)) {
			return null;
		}

		final String tokenString = ReaderState.convertTokensToString(tokenAttributes);

		final int numberOfRationalParts = 2;
		final String[] rationalParts = tokenString.split("/", numberOfRationalParts);

		final String numeratorPart = rationalParts[0];

		// Numerator cannot contain a DECIMAL
		if (numeratorPart.contains(".")) {
			return null;
		}

		final Integer exponentToken = ReaderState.getTokenByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		final String numeratorTokenString = getFloatTokenString(numeratorPart, exponentToken);
		final BigDecimal numeratorBigDecimal = new BigDecimal(numeratorTokenString);

		final String denominatorTokenString = getFloatTokenString(rationalParts[1], exponentToken);
		final BigDecimal denominatorBigDecimal = new BigDecimal(denominatorTokenString);

		BigDecimal bigDecimal = numeratorBigDecimal.divide(denominatorBigDecimal, MathContext.DECIMAL128);

		final int scale = bigDecimal.scale();
		if (scale < 1) {
			bigDecimal = bigDecimal.setScale(1, RoundingMode.HALF_UP);
		}

		final jcl.types.Float aFloat = getFloatType(exponentToken);
		return new FloatStruct(aFloat, bigDecimal);
	}
}
