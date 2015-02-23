/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.compiler.real.element.SimpleElement;
import jcl.numbers.IntegerStruct;
import jcl.reader.AttributeType;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableCase;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
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

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6813793813858944602L;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public SimpleElement process(final TokenBuilder tokenBuilder) {

		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();
		int codePoint = readResult.getResult(); // This will not be 'null'. We check for EOFs after each 'read'.

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final ReadtableCase readtableCase = readtable.getReadtableCase();

		final IntegerStruct readBase = ReaderVariables.READ_BASE.getValue();
		final AttributeType attributeType = readtable.getAttributeType(codePoint, readBase);

		codePoint = ReaderState.properCaseCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		return readerStateMediator.readEvenMultipleEscape(tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
