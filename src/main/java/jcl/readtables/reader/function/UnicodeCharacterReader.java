package jcl.readtables.reader.function;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.Reader;

import static jcl.readtables.reader.function.FunctionReaderUtils.getNextCodePoint;
import static jcl.readtables.reader.function.FunctionReaderUtils.isWhitespace;

public class UnicodeCharacterReader {

	private final Reader reader;

	public UnicodeCharacterReader(final Reader reader) {
		this.reader = reader;
	}

	public int readUnicodeChar() {

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