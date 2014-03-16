package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.MacroFunctionReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#r' Lisp reader macro.
 */
public class SharpRReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);
		return reader.readIntegerToken(numArg);
	}
}
