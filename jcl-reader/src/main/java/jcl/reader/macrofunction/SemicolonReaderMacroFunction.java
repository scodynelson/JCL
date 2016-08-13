/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the ';' Lisp reader macro.
 */
@Component
public class SemicolonReaderMacroFunction extends ReaderMacroFunctionImpl {

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.SEMICOLON, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readResult = reader.readChar(false, null, false);
		Integer nextCodePoint = readResult.getResult();
		while (!readResult.isEof() && (nextCodePoint.intValue() != CodePointConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(nextCodePoint);

			readResult = reader.readChar(false, null, false);
			nextCodePoint = readResult.getResult();
		}

//		final String stringValue = stringBuilder.toString();
//		return new CommentStruct(stringValue);
		return NILStruct.INSTANCE;
	}
}
