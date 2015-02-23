/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.compiler.real.element.SimpleElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * Step 10 of the Reader Algorithm.
 * <p>
 * An entire token has been accumulated. The object represented by the token is returned as the result of the read
 * operation, or an error of type reader-error is signaled if the token is not of valid syntax.
 * </p>
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Number/PotentialNumber
 * 2) Symbol
 * 3) Package with a Symbol
 * </p>
 */
@Component
class TokenAccumulatedReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6758283004867508557L;

	/**
	 * {@link NumberTokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private NumberTokenAccumulatedReaderState numberTokenAccumulatedReaderState;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public SimpleElement process(final TokenBuilder tokenBuilder) {

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final List<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		if (tokenAttributes.isEmpty()) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		final String tokenString = ReaderState.convertTokensToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		}

		return numberTokenAccumulatedReaderState.process(tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
