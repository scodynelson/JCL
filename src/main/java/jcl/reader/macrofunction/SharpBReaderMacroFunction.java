package jcl.reader.macrofunction;

import jcl.reader.MacroFunctionReader;
import jcl.reader.ReaderMacroFunction;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

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
