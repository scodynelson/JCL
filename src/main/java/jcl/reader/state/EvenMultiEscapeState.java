/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.numbers.IntegerStruct;
import jcl.reader.AttributeType;
import jcl.reader.ReadtableCase;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.SyntaxType;
import jcl.streams.ReadPeekResult;

/**
 * Step 8 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an even number of multiple escape characters have been encountered.
 * If at end of file, step 10 is entered. Otherwise, a character, y, is read, and one of the following actions is
 * performed according to its syntax type:
 * <tab>
 * <p>
 * If y is a constituent or non-terminating macro character:
 * <tab>
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
 * </tab>
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
 * </tab>
 * </p>
 */
final class EvenMultiEscapeState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new EvenMultiEscapeState();

	/**
	 * Private constructor.
	 */
	private EvenMultiEscapeState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		ReadPeekResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.isEof()) {
			readerStateMediator.readTokenAccumulated(reader, tokenBuilder);
		}

		int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			final ReadtableCase readtableCase = readtable.getReadtableCase();

			final IntegerStruct readBase = ReaderVariables.READ_BASE.getValue();
			final AttributeType attributeType = readtable.getAttributeType(codePoint, readBase);

			codePoint = State.properCaseCodePoint(codePoint, attributeType, readtableCase);
			tokenBuilder.addToTokenAttributes(codePoint, attributeType);

			readerStateMediator.readEvenMultipleEscape(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.isEof()) {
				readerStateMediator.readIllegalCharacter(reader, tokenBuilder);
			} else {
				codePoint = readResult.getResult();
				tokenBuilder.setPreviousReadCharacter(codePoint);
				tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

				readerStateMediator.readEvenMultipleEscape(reader, tokenBuilder);
			}
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			readerStateMediator.readOddMultipleEscape(reader, tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.WHITESPACE)) {
			// NOTE from CLHS in regarding 'SyntaxType.WHITESPACE' characters:
			//      If a command interpreter takes single-character commands, but occasionally reads an object then if
			//      the whitespace[2] after a symbol is not discarded it might be interpreted as a command some time
			//      later after the symbol had been read.
			reader.unreadChar(codePoint);
			readerStateMediator.readTokenAccumulated(reader, tokenBuilder);
		} else {
			readerStateMediator.readIllegalCharacter(reader, tokenBuilder);
		}
	}
}
