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
 * Step 8 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an even number of multiple escape characters have been encountered.
 * If at end of file, step 10 is entered. Otherwise, a character, y, is read, and one of the following actions is
 * performed according to its syntax type:
 * <p>
 * If y is a constituent or non-terminating macro character:
 * <p>
 * -- If y is a character with case, it might be replaced with the corresponding character of the opposite case,
 * depending on the readtable case of the current readtable.
 * </p>
 * <p>
 * -- Y is appended to the token being built.
 * </p>
 * <p>
 * -- Step 8 is repeated.
 * </p>
 * <p>
 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
 * if at end of file. Z is treated as if it is a constituent whose only constituent trait is alphabetic. Z is appended
 * to the token being built, and step 8 is repeated.
 * </p>
 * <p>
 * If y is a multiple escape character, then step 9 is entered.
 * </p>
 * <p>
 * If y is an invalid character, an error of type reader-error is signaled.
 * </p>
 * <p>
 * If y is a terminating macro character, then it terminates the token. First the character y is unread, and then step
 * 10 is entered.
 * </p>
 * <p>
 * If y is a whitespace character, then it terminates the token. First the character y is unread if appropriate, and
 * then step 10 is entered.
 * </p>
 */
@Component
class EvenMultiEscapeReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	/**
	 * {@link SymbolTokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private SymbolTokenAccumulatedReaderState symbolTokenAccumulatedReaderState;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final Reader reader = tokenBuilder.getReader();

		ReadPeekResult readResult = reader.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			final boolean isMultiEscapedToken = tokenBuilder.isMultiEscapedToken();
			if (isMultiEscapedToken) {
				return symbolTokenAccumulatedReaderState.process(tokenBuilder);
			} else {
				return readerStateMediator.readTokenAccumulated(tokenBuilder);
			}
		}

		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			return readerStateMediator.readConstituent(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, true);
			tokenBuilder.setPreviousReadResult(readResult);

			if (readResult.isEof()) {
				return readerStateMediator.readIllegalCharacter(tokenBuilder);
			}

			codePoint = readResult.getResult();
			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readerStateMediator.readEvenMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			return readerStateMediator.readOddMultipleEscape(tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.WHITESPACE)) {
			// NOTE from CLHS in regarding 'SyntaxType.WHITESPACE' characters:
			//      If a command interpreter takes single-character commands, but occasionally reads an object then if
			//      the whitespace[2] after a symbol is not discarded it might be interpreted as a command some time
			//      later after the symbol had been read.
			reader.unreadChar(codePoint);

			final boolean isMultiEscapedToken = tokenBuilder.isMultiEscapedToken();
			if (isMultiEscapedToken) {
				return symbolTokenAccumulatedReaderState.process(tokenBuilder);
			} else {
				return readerStateMediator.readTokenAccumulated(tokenBuilder);
			}
		} else {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(readerStateMediator)
		                            .append(symbolTokenAccumulatedReaderState)
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
		final EvenMultiEscapeReaderState rhs = (EvenMultiEscapeReaderState) obj;
		return new EqualsBuilder().append(readerStateMediator, rhs.readerStateMediator)
		                          .append(symbolTokenAccumulatedReaderState, rhs.symbolTokenAccumulatedReaderState)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(readerStateMediator)
		                                                                .append(symbolTokenAccumulatedReaderState)
		                                                                .toString();
	}
}
