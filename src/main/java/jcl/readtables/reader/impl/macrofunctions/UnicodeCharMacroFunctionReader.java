package jcl.readtables.reader.impl.macrofunctions;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

public class UnicodeCharMacroFunctionReader extends BaseMacroFunctionReader {

	public UnicodeCharMacroFunctionReader(final StateReader stateReader) {
		super(stateReader);
	}

	public int readUnicodeChar() {

		final StringBuilder unicodeCharBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = stateReader.readChar();
		int readChar = readResult.getResult();

		while (!isSyntaxType(stateReader, readChar, SyntaxType.WHITESPACE)) {
			unicodeCharBuilder.appendCodePoint(readChar);

			readResult = stateReader.readChar();
			readChar = readResult.getResult();
		}

		final String unicodeCharString = unicodeCharBuilder.toString();
		try {
			final int codePoint = Integer.parseInt(unicodeCharString, 16);
			if (!Character.isValidCodePoint(codePoint)) {
				throw new ReaderErrorException("0x" + unicodeCharString + " is not a valid code point.");
			}
			return codePoint;
		} catch (final NumberFormatException nfe) {
			throw new ReaderErrorException('"' + unicodeCharString + "\" does not represent a hexadecimal integer.", nfe);
		}
	}
}
