/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.streams.ReadPeekResult;

/**
 * Reader Macro Function for handling the reading unicode character.
 */
final class UnicodeCharacterReaderMacroFunction {

	/**
	 * The Unicode radix value used to parse unicode characters into proper character code points.
	 */
	private static final int UNICODE_RADIX = 16;

	/**
	 * Private constructor.
	 */
	private UnicodeCharacterReaderMacroFunction() {
	}

	/**
	 * Reads in and returns the properly read in Unicode character token. A {@link ReaderErrorException} is thrown if
	 * the character is not a proper Unicode character.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the Unicode character token
	 *
	 * @return a Unicode character code point
	 */
	static int readUnicodeCharacter(final Reader reader) {
		final StringBuilder unicodeCharacterBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = reader.readChar(true, NullStruct.INSTANCE, false);
		int codePoint = readResult.getResult();
		while (!ReaderMacroFunctionUtil.isWhitespace(codePoint)) {
			unicodeCharacterBuilder.appendCodePoint(codePoint);

			readResult = reader.readChar(true, NullStruct.INSTANCE, false);
			codePoint = readResult.getResult();
		}

		final String unicodeCharacterString = unicodeCharacterBuilder.toString();
		try {
			final int unicodeCodePoint = Integer.parseInt(unicodeCharacterString, UNICODE_RADIX);
			if (!Character.isValidCodePoint(unicodeCodePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharacterString + " is not a valid code point.");
			}
			return unicodeCodePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharacterString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}
}
