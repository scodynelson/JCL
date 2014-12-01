/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '"..."' Lisp reader macro.
 */
@Component
public class QuotationMarkReaderMacroFunction extends UnicodeCharacterReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.QUOTATION_MARK, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.QUOTATION_MARK;

		final StringBuilder stringBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = reader.readChar();
		int readChar = readResult.getResult();

		while (readChar != CharacterConstants.QUOTATION_MARK) {
			if (readChar == CharacterConstants.BACKSLASH) {
				handleEscapedCharacter(reader, stringBuilder);
			} else {
				stringBuilder.appendCodePoint(readChar);
			}

			// NOTE: This will throw errors when it reaches an EOF
			readResult = reader.readChar();
			readChar = readResult.getResult();
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		try {
			return new StringStruct(stringValue);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating string.", e);
		}
	}

	/**
	 * Handles escaped characters during a read operation for '"..."'.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read tokens
	 * @param stringBuilder
	 * 		the {@link StringBuilder} used to build the final token
	 */
	private static void handleEscapedCharacter(final Reader reader, final StringBuilder stringBuilder) {
		int readChar = CharacterConstants.BACKSLASH;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult tmpReadResult = reader.readChar();
		final int tmpChar = tmpReadResult.getResult();
		if ((tmpChar == CharacterConstants.LATIN_SMALL_LETTER_U)
				|| (tmpChar == CharacterConstants.LATIN_CAPITAL_LETTER_U)) {

			final ReadPeekResult nextTmpReadResult = reader.readChar();
			final int nextTmpChar = nextTmpReadResult.getResult();
			if (nextTmpChar == CharacterConstants.PLUS_SIGN) {
				readChar = readUnicodeCharacter(reader);
				stringBuilder.appendCodePoint(readChar);
			} else {
				// NOTE: Order matters here!!
				stringBuilder.appendCodePoint(readChar);
				stringBuilder.appendCodePoint(tmpChar);
				stringBuilder.appendCodePoint(nextTmpChar);
			}
		} else {
			// NOTE: Order matters here!!
			stringBuilder.appendCodePoint(readChar);
			stringBuilder.appendCodePoint(tmpChar);
		}
	}
}
