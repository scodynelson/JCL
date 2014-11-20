package jcl.reader.macrofunction;

import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

abstract class UnicodeCharacterReaderMacroFunction extends ReaderMacroFunction {

	protected static Integer process(final Reader reader) {
		final StringBuilder unicodeCharBuilder = new StringBuilder();

		int codePoint = getNextCodePoint(reader);
		while (!isWhitespace(reader, codePoint)) {
			unicodeCharBuilder.appendCodePoint(codePoint);
			codePoint = getNextCodePoint(reader);
		}

		final String unicodeCharString = unicodeCharBuilder.toString();
		try {
			final int unicodeCodePoint = Integer.parseInt(unicodeCharString, 16);
			if (!Character.isValidCodePoint(unicodeCodePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharString + " is not a valid code point.");
			}
			return unicodeCodePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}
}
