package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Implements the '#x' Lisp reader macro.
 */
public class SharpXReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_X) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_X);
		return reader.readIntegerToken(16);
	}
}
