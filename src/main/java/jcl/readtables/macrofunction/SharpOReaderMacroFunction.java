package jcl.readtables.macrofunction;

import jcl.readtables.reader.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

/**
 * Implements the '#o' Lisp reader macro.
 */
public class SharpOReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_O) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_O);
		return reader.readIntegerToken(8);
	}
}
