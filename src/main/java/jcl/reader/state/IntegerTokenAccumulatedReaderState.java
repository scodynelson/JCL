/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.reader.AttributeType;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import org.springframework.stereotype.Component;

/**
 * Sub-piece of Reader algorithm part 10.1, used to produce an {@link IntegerStruct} output when a rational token is
 * supplied with no {@link AttributeType#RATIOMARKER} nor {@link AttributeType#DECIMAL}.
 */
@Component
public class IntegerTokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8986705547108246215L;

	/**
	 * The list of {@link AttributeType}s that should only be first if present in a numeric token.
	 */
	private static final List<AttributeType> FIRST_ONLY_ATTRIBUTES = Arrays.asList(AttributeType.PLUS, AttributeType.MINUS);

	@Override
	public NumberStruct process(final TokenBuilder tokenBuilder) {

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

		final String tokenString = ReaderState.convertTokenAttributesToString(tokenAttributes);
		final int currentRadix = ReaderVariables.READ_BASE.getValue().getBigInteger().intValueExact();

		final BigInteger bigInteger = new BigInteger(tokenString, currentRadix);
		return new IntegerStruct(bigInteger);
	}
}
