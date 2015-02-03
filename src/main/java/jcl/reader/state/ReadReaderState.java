/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.struct.SyntaxType;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 1 of the Reader Algorithm.
 * <p>
 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
 * </p>
 */
@Component
class ReadReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5191264706029366940L;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadPeekResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.isEof()) {
			ReaderState.handleEndOfFile(tokenBuilder, "ReadReaderState");
			return;
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if (syntaxType == SyntaxType.WHITESPACE) {
			readerStateMediator.readWhitespace(reader, tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			readerStateMediator.readMacroCharacter(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			readerStateMediator.readSingleEscape(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			readerStateMediator.readMultipleEscape(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			readerStateMediator.readConstituent(reader, tokenBuilder);
		} else {
			readerStateMediator.readIllegalCharacter(reader, tokenBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
