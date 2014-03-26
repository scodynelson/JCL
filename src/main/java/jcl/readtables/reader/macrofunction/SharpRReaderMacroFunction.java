package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.impl.macrofunctions.IntegerMacroFunctionReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#r' Lisp reader macro.
 */
public class SharpRReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		final IntegerMacroFunctionReader macroFunctionReader = new IntegerMacroFunctionReader(reader);
		return macroFunctionReader.readIntegerToken(numArg);
	}
}
