/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.numbers.IntegerStruct;
import jcl.reader.struct.AttributeType;
import jcl.reader.struct.ReadtableCase;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import org.springframework.stereotype.Component;

/**
 * Step 7 of the Reader Algorithm.
 * <p>
 * If x is a constituent character, then it begins a token. After the token is read in, it will be interpreted either
 * as a Lisp object or as being of invalid syntax. If the token represents an object, that object is returned as the
 * result of the read operation. If the token is of invalid syntax, an error is signaled. If x is a character with
 * case, it might be replaced with the corresponding character of the opposite case, depending on the readtable case of
 * the current readtable. X is used to begin a token, and step 8 is entered.
 * </p>
 */
@Component
class ConstituentReaderState implements ReaderState {

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (ReaderState.isEndOfFileCharacter(codePoint)) {
			ReaderState.handleEndOfFile(tokenBuilder, "ConstituentReaderState");
			return;
		}

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final ReadtableCase readtableCase = readtable.getReadtableCase();

		final IntegerStruct readBase = ReaderVariables.READ_BASE.getValue();
		final AttributeType attributeType = readtable.getAttributeType(codePoint, readBase);

		codePoint = ReaderState.properCaseCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		readerStateMediator.readEvenMultipleEscape(reader, tokenBuilder);
	}
}
