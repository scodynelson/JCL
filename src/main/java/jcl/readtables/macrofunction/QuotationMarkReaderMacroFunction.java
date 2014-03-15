package jcl.readtables.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.arrays.StringStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.syntax.reader.ReadResult;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '"..."' Lisp reader macro.
 */
public class QuotationMarkReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.QUOTATION_MARK;

		final StringBuilder stringBuilder = new StringBuilder();

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = reader.readChar();
		int readChar = readResult.getResult();

		while (readChar != CharacterConstants.QUOTATION_MARK) {
			if (readChar == CharacterConstants.BACKSLASH) {

				// NOTE: This will throw errors when it reaches an EOF
				final ReadResult tmpReadResult = reader.readChar();
				final int tmpChar = tmpReadResult.getResult();
				if ((tmpChar == CharacterConstants.LATIN_SMALL_LETTER_U)
						|| (tmpChar == CharacterConstants.LATIN_CAPITAL_LETTER_U)) {

					final ReadResult nextTmpReadResult = reader.readChar();
					final int nextTmpChar = nextTmpReadResult.getResult();
					if (nextTmpChar == CharacterConstants.PLUS_SIGN) {
						readChar = reader.readUnicodeChar();
						stringBuilder.appendCodePoint(readChar);
					} else {
						// NOTE: Order matters here!!
						stringBuilder.appendCodePoint(readChar);
						stringBuilder.appendCodePoint(tmpChar);
						stringBuilder.appendCodePoint(nextTmpChar);
					}
				} else {
					// NOTE: Order matters here!!
					stringBuilder.appendCodePoint(readChar);
					stringBuilder.appendCodePoint(tmpChar);
				}
			} else {
				stringBuilder.appendCodePoint(readChar);
			}

			// NOTE: This will throw errors when it reaches an EOF
			readResult = reader.readChar();
			readChar = readResult.getResult();
		}

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		try {
			return new StringStruct(stringValue);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating string.", e);
		}
	}
}
