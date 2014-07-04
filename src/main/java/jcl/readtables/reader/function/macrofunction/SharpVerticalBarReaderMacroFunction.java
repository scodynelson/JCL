package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.Reader;
import jcl.structs.comments.CommentStruct;
import jcl.syntax.CharacterConstants;
import jcl.syntax.reader.ReadResult;
import jcl.readtables.reader.ReadSuppressVariable;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
public class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.VERTICAL_LINE;

		int level = 1;

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult prevReadResult = reader.readChar();
		ReadResult nextReadResult = reader.readChar();

		final StringBuilder stringBuilder = new StringBuilder();
		while (true) {

			final int prevChar = prevReadResult.getResult();
			final int nextChar = nextReadResult.getResult();
			if ((prevChar == CharacterConstants.VERTICAL_LINE) && (nextChar == CharacterConstants.NUMBER_SIGN)) {
				level -= 1;
				if (level == 0) {
					break;
				} else {
					stringBuilder.appendCodePoint(prevChar);
				}
			} else if ((prevChar == CharacterConstants.NUMBER_SIGN) && (nextChar == CharacterConstants.VERTICAL_LINE)) {
				stringBuilder.appendCodePoint(prevChar);
				stringBuilder.appendCodePoint(nextChar);

				// NOTE: This will throw errors when it reaches an EOF
				nextReadResult = reader.readChar();
				level += 1;
			} else {
				stringBuilder.appendCodePoint(prevChar);
			}

			// NOTE: This will throw errors when it reaches an EOF
			prevReadResult = nextReadResult;
			nextReadResult = reader.readChar();
		}

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return CommentStruct.getStruct(stringValue);
	}
}