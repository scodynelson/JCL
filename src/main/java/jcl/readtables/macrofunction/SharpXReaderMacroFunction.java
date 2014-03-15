package jcl.readtables.macrofunction;

import jcl.readtables.reader.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

/**
 * Implements the '#x' Lisp reader macro.
 */
public class SharpXReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_X) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_X);
		return reader.readIntegerToken(16);
	}
}
