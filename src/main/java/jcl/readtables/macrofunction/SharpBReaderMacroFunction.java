package jcl.readtables.macrofunction;

import jcl.LispStruct;
import jcl.readtables.MacroFunctionReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#b' Lisp reader macro.
 */
public class SharpBReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);
		return reader.readIntegerToken(2);
	}
}
