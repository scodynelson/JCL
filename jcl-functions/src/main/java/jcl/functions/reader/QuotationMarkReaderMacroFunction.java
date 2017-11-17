/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '"..."' Lisp reader macro.
 */
@Component
public class QuotationMarkReaderMacroFunction extends ReaderMacroFunctionImpl {

	public QuotationMarkReaderMacroFunction() {
		super("QUOTATION-MARK");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.QUOTATION_MARK, this, false);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.QUOTATION_MARK;

		final StringBuilder stringBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, true);
		int nextCodePoint = readResult.getResult();

		while (nextCodePoint != CodePointConstants.QUOTATION_MARK) {
			if (nextCodePoint == CodePointConstants.BACKSLASH) {
				handleEscapedCharacter(inputStreamStruct, stringBuilder);
			} else {
				stringBuilder.appendCodePoint(nextCodePoint);
			}

			// NOTE: This will throw errors when it reaches an EOF
			readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, true);
			nextCodePoint = readResult.getResult();
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		final String stringValue = stringBuilder.toString();
		return StringStruct.toLispString(stringValue);
	}

	/**
	 * Handles escaped characters during a read operation for '"..."'.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read tokens from
	 * @param stringBuilder
	 * 		the {@link StringBuilder} used to build the final token
	 */
	private static void handleEscapedCharacter(final InputStreamStruct inputStreamStruct, final StringBuilder stringBuilder) {
		int codePoint = CodePointConstants.BACKSLASH;

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult tempReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, true);
		final int tempCodePoint = tempReadResult.getResult();
		if ((tempCodePoint == CodePointConstants.LATIN_SMALL_LETTER_U)
				|| (tempCodePoint == CodePointConstants.LATIN_CAPITAL_LETTER_U)) {

			final ReadPeekResult nextTempReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE, true);
			final int nextTempCodePoint = nextTempReadResult.getResult();
			if (nextTempCodePoint == CodePointConstants.PLUS_SIGN) {
				codePoint = UnicodeCharacterReaderMacroFunction.readUnicodeCharacter(inputStreamStruct);
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
