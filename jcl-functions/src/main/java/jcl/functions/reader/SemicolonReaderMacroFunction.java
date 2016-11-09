/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.reader.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the ';' Lisp reader macro.
 */
@Component
public class SemicolonReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public SemicolonReaderMacroFunction(final Reader reader) {
		super("SEMICOLON");
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.SEMICOLON, this, false);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readResult = reader.readChar(inputStreamStruct, false, null, false);
		Integer nextCodePoint = readResult.getResult();
		while (!readResult.isEof() && (nextCodePoint.intValue() != CodePointConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(nextCodePoint);

			readResult = reader.readChar(inputStreamStruct, false, null, false);
			nextCodePoint = readResult.getResult();
		}

//		final String stringValue = stringBuilder.toString();
//		return new CommentStruct(stringValue);
		return NILStruct.INSTANCE;
	}
}
