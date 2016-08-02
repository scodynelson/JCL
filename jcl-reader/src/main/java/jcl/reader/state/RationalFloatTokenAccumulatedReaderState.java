/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.LinkedList;

import jcl.lang.FloatStruct;
import jcl.lang.NumberStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.number.NumberUtils;
import jcl.lang.readtable.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.type.FloatType;
import org.springframework.stereotype.Component;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link FloatStruct} output when a rational token is
 * supplied with both an {@link AttributeType#RATIOMARKER} and {@link AttributeType#DECIMAL}. This means using the
 * correct exponential division using {@link MathContext#DECIMAL128} and {@link RoundingMode#HALF_UP} to produce an
 * accurate float result.
 */
@Component
public class RationalFloatTokenAccumulatedReaderState extends FloatTokenAccumulatedReaderState {

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

		final String tokenString = ReaderState.convertTokenAttributesToString(tokenAttributes);

		final int numberOfRationalParts = 2;
		final String[] rationalParts = tokenString.split("/", numberOfRationalParts);

		final String numeratorPart = rationalParts[0];

		// Numerator cannot contain a DECIMAL
		if (numeratorPart.contains(".")) {
			return null;
		}

		final Integer exponentTokenCodePoint = ReaderState.getTokenCodePointByAttribute(tokenAttributes, AttributeType.EXPONENTMARKER);

		final String numeratorTokenString = getFloatTokenString(numeratorPart, exponentTokenCodePoint);
		final BigDecimal numeratorBigDecimal = NumberUtils.bigDecimalValue(numeratorTokenString);

		final String denominatorTokenString = getFloatTokenString(rationalParts[1], exponentTokenCodePoint);
		final BigDecimal denominatorBigDecimal;
		try {
			denominatorBigDecimal = NumberUtils.bigDecimalValue(denominatorTokenString);
		} catch (final NumberFormatException ignore) {
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
//			} catch (final NumberFormatException ignore) {
//				return null;
//			}
//		} else {
//			try {
//				final Float f = bigDecimal.floatValue();
//				return SingleFloatStruct.valueOf(f);
//			} catch (final NumberFormatException ignore) {
//				return null;
//			}
//		}
	}
}
