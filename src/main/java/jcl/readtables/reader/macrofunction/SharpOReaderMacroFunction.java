package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.IntegerMacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#o' Lisp reader macro.
 */
public class SharpOReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_O) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_O);

		final IntegerMacroFunctionReader macroFunctionReader = new IntegerMacroFunctionReader(reader);
		return macroFunctionReader.readIntegerToken(8);
	}
}
