package jcl.reader.macrofunction;

import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Reader Macro Function for handling the reading unicode character.
 */
abstract class UnicodeCharacterReaderMacroFunction extends ReaderMacroFunction {

	private static final int SIXTEEN = 16;

	/**
	 * Reads in and returns the properly read in Unicode character token. A {@link ReaderErrorException} is thrown if
	 * the character is not a proper Unicode character.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the Unicode character token
	 *
	 * @return a Unicode character code point
	 */
	protected static int readUnicodeCharacter(final Reader reader) {
		final StringBuilder unicodeCharBuilder = new StringBuilder();

		int codePoint = getNextCodePoint(reader);
		while (!isWhitespace(reader, codePoint)) {
			unicodeCharBuilder.appendCodePoint(codePoint);
			codePoint = getNextCodePoint(reader);
		}

		final String unicodeCharString = unicodeCharBuilder.toString();
		try {
			final int unicodeCodePoint = Integer.parseInt(unicodeCharString, SIXTEEN);
			if (!Character.isValidCodePoint(unicodeCodePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharString + " is not a valid code point.");
			}
			return unicodeCodePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}
}
