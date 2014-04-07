package jcl.readtables.reader.function;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.Reader;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

public class UnicodeCharMacroFunctionReader {

	private final Reader reader;

	public UnicodeCharMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public int readUnicodeChar() {

		final StringBuilder unicodeCharBuilder = new StringBuilder();

		int codePoint = getNextCodePoint();
		while (!isWhitespace(codePoint)) {
			unicodeCharBuilder.appendCodePoint(codePoint);
			codePoint = getNextCodePoint();
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

	private int getNextCodePoint() {
		// NOTE: This will throw errors when it reaches an EOF
		final ReadResult readResult = reader.readChar();
		return readResult.getResult();
	}

	private boolean isWhitespace(final int codePoint) {
		return MacroFunctionReaderUtils.isSyntaxType(reader, codePoint, SyntaxType.WHITESPACE);
	}
}
