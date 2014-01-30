package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.comments.CommentStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.ReadResult;
import jcl.types.Variable;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
public class SharpVerticalBarReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
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

		if (Variable.ReadSuppress) {
			return null;
		} else {
			final String stringValue = stringBuilder.toString();
			return CommentStruct.getStruct(stringValue);
		}
	}
}
