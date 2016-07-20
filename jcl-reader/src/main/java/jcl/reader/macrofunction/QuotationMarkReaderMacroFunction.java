/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.stereotype.Component;

/**
 * Implements the '"..."' Lisp reader macro.
 */
@Component
public class QuotationMarkReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CharacterConstants.QUOTATION_MARK, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.QUOTATION_MARK;

		final StringBuilder stringBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = reader.readChar(true, NILStruct.INSTANCE, true);
		int nextCodePoint = readResult.getResult();

		while (nextCodePoint != CharacterConstants.QUOTATION_MARK) {
			if (nextCodePoint == CharacterConstants.BACKSLASH) {
				handleEscapedCharacter(reader, stringBuilder);
			} else {
				stringBuilder.appendCodePoint(nextCodePoint);
			}

			// NOTE: This will throw errors when it reaches an EOF
			readResult = reader.readChar(true, NILStruct.INSTANCE, true);
			nextCodePoint = readResult.getResult();
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		final String stringValue = stringBuilder.toString();
		return new StringStruct(stringValue);
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
		int codePoint = CharacterConstants.BACKSLASH;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult tempReadResult = reader.readChar(true, NILStruct.INSTANCE, true);
		final int tempCodePoint = tempReadResult.getResult();
		if ((tempCodePoint == CharacterConstants.LATIN_SMALL_LETTER_U)
				|| (tempCodePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U)) {

			final ReadPeekResult nextTempReadResult = reader.readChar(true, NILStruct.INSTANCE, true);
			final int nextTempCodePoint = nextTempReadResult.getResult();
			if (nextTempCodePoint == CharacterConstants.PLUS_SIGN) {
				codePoint = UnicodeCharacterReaderMacroFunction.readUnicodeCharacter(reader);
				stringBuilder.appendCodePoint(codePoint);
			} else {
				// NOTE: Order matters here!!
				stringBuilder.appendCodePoint(codePoint);
				stringBuilder.appendCodePoint(tempCodePoint);
				stringBuilder.appendCodePoint(nextTempCodePoint);
			}
		} else {
			// NOTE: Order matters here!!
			stringBuilder.appendCodePoint(codePoint);
			stringBuilder.appendCodePoint(tempCodePoint);
		}
	}
}