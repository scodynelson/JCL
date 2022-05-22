/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import lombok.experimental.UtilityClass;

/**
 * Reader Macro Function for handling the reading unicode character.
 */
@UtilityClass
final class UnicodeCharacterReaderMacroFunction {

	/**
	 * The Unicode radix value used to parse unicode characters into proper character code points.
	 */
	private static final int UNICODE_RADIX = 16;

	/**
	 * Reads in and returns the properly read in Unicode character token. A {@link ReaderErrorException} is thrown if
	 * the character is not a proper Unicode character.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read the Unicode character token from
	 *
	 * @return a Unicode character code point
	 */
	static int readUnicodeCharacter(final InputStreamStruct inputStreamStruct) {
		final ExtendedTokenReaderMacroFunction.ReadExtendedToken readExtendedToken =
				ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, true);

		final String unicodeCharacterString = readExtendedToken.getTokenString();
		try {
			final int unicodeCodePoint = Integer.parseInt(unicodeCharacterString, UNICODE_RADIX);
			if (!Character.isValidCodePoint(unicodeCodePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharacterString + " is not a valid code point.");
			}
			return unicodeCodePoint;
		} catch (final NumberFormatException nfe) {
			final String message = '"' + unicodeCharacterString + "\" does not represent a hexadecimal integer.";
			throw new ReaderErrorException(message, nfe);
		}
	}
}
