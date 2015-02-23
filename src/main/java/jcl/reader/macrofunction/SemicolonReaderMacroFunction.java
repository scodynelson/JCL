/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.CommentElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the ';' Lisp reader macro.
 */
@Component
public class SemicolonReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1979907255889244298L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.SEMICOLON, this, false);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readResult = reader.readChar(false, null, false);
		Integer readChar = readResult.getResult();
		while (!readResult.isEof() && (readChar.intValue() != CharacterConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(readChar);

			readResult = reader.readChar(false, null, false);
			readChar = readResult.getResult();
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return new CommentElement(stringValue);
	}
}
