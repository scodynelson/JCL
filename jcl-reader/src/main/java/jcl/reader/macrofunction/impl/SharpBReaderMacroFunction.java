package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Implements the '#b' Lisp reader macro.
 */
public class SharpBReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);
		return reader.readIntegerToken(2);
	}
}
