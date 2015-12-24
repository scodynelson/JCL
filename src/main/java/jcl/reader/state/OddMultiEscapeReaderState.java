/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.reader.AttributeType;
import jcl.reader.Reader;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.struct.SyntaxType;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 9 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an odd number of multiple escape characters have been encountered.
 * If at end of file, an error of type end-of-file is signaled. Otherwise, a character, y, is read, and one of the
 * following actions is performed according to its syntax type:
 * <p>
 * If y is a constituent, macro, or whitespace character, y is treated as a constituent whose only constituent trait is
 * alphabetic. Y is appended to the token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
 * if at end of file. Z is treated as a constituent whose only constituent trait is alphabetic. Z is appended to the
 * token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a multiple escape character, then step 8 is entered.
 * </p>
 * <p>
 * If y is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
@Component
class OddMultiEscapeReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2279541006810083230L;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final Reader reader = tokenBuilder.getReader();

		ReadPeekResult readResult = reader.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readerStateMediator.readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, true);
			tokenBuilder.setPreviousReadResult(readResult);

			if (readResult.isEof()) {
				return readerStateMediator.readIllegalCharacter(tokenBuilder);
			}

			codePoint = readResult.getResult();
			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readerStateMediator.readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			return readerStateMediator.readEvenMultipleEscape(tokenBuilder);
		} else {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(readerStateMediator)
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
		final OddMultiEscapeReaderState rhs = (OddMultiEscapeReaderState) obj;
		return new EqualsBuilder().append(readerStateMediator, rhs.readerStateMediator)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(readerStateMediator)
		                                                                .toString();
	}
}
