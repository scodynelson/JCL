/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.reader.Reader;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.streams.ReadPeekResult;

/**
 * Reader Macro Function for handling the reading unicode character.
 */
abstract class UnicodeCharacterReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * The Unicode radix value used to parse unicode characters into proper character code points.
	 */
	private static final int UNICODE_RADIX = 16;

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
		final StringBuilder unicodeCharBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult readResult = reader.readChar();
		int codePoint = readResult.getResult();
		while (!isWhitespace(reader, codePoint)) {
			unicodeCharBuilder.appendCodePoint(codePoint);

			readResult = reader.readChar();
			codePoint = readResult.getResult();
		}

		final String unicodeCharString = unicodeCharBuilder.toString();
		try {
			final int unicodeCodePoint = Integer.parseInt(unicodeCharString, UNICODE_RADIX);
			if (!Character.isValidCodePoint(unicodeCodePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharString + " is not a valid code point.");
			}
			return unicodeCodePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}
}
