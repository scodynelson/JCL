/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.Reader;

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
		final ExtendedTokenReaderMacroFunction.ReadExtendedToken readExtendedToken =
				ExtendedTokenReaderMacroFunction.readExtendedToken(reader, true);

		final String unicodeCharacterString = readExtendedToken.getTokenString();
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
