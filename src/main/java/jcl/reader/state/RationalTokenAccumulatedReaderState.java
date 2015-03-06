/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.reader.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.math3.fraction.BigFraction;
import org.springframework.stereotype.Component;

import java.math.BigInteger;
import java.util.LinkedList;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce a {@link RatioStruct} output when a rational token is
 * supplied with an {@link AttributeType#RATIOMARKER}. This will also produce an {@link IntegerStruct} when the
 * resulting {@link BigInteger} has a denominator of {@link BigInteger#ONE}.
 */
@Component
public class RationalTokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1560698912123354546L;

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

		final int currentRadix = ReaderVariables.READ_BASE.getValue().getBigInteger().intValueExact();

		final BigInteger numerator = new BigInteger(rationalParts[0], currentRadix);
		final BigInteger denominator = new BigInteger(rationalParts[1], currentRadix);

		final BigFraction rational = new BigFraction(numerator, denominator);

		final BigInteger fractionDenominator = rational.getDenominator();
		if (BigInteger.ONE.equals(fractionDenominator)) {
			final BigInteger fractionNumerator = rational.getNumerator();
			return new IntegerStruct(fractionNumerator);
		}

		return new RatioStruct(rational);
	}
}