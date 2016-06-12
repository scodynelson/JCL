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
import jcl.symbols.NILStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ReadReaderState.class);

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

		final ReadPeekResult readResult = reader.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		final int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		final LispStruct token;

		if (syntaxType == SyntaxType.WHITESPACE) {
			token = readerStateMediator.readWhitespace(tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			token = readerStateMediator.readMacroCharacter(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			token = readerStateMediator.readSingleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			token = readerStateMediator.readMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			token = readerStateMediator.readConstituent(tokenBuilder);
		} else {
			token = readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", token);
			}
			return NILStruct.INSTANCE;
		}
		return token;
	}
}
