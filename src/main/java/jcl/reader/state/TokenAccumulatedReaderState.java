/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.TokenAttribute;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return null;
		}

		final List<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();

		final String tokenString = ReaderState.convertTokenAttributesToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		}

		return numberTokenAccumulatedReaderState.process(tokenBuilder);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(numberTokenAccumulatedReaderState)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final TokenAccumulatedReaderState rhs = (TokenAccumulatedReaderState) obj;
		return new EqualsBuilder().append(numberTokenAccumulatedReaderState, rhs.numberTokenAccumulatedReaderState)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(numberTokenAccumulatedReaderState)
		                                                                .toString();
	}
}
