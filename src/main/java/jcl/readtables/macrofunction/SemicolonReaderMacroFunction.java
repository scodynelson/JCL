package jcl.readtables.macrofunction;

import jcl.LispStruct;
import jcl.readtables.MacroFunctionReader;
import jcl.structs.comments.CommentStruct;
import jcl.syntax.CharacterConstants;
import jcl.syntax.reader.ReadResult;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the ';' Lisp reader macro.
 */
public class SemicolonReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadResult readResult = reader.readChar(false, null, false);
		Integer readChar = readResult.getResult();
		while (!readResult.wasEOF() && (readChar.intValue() != CharacterConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(readChar);

			readResult = reader.readChar(false, null, false);
			readChar = readResult.getResult();
		}

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return CommentStruct.getStruct(stringValue);
	}
}
