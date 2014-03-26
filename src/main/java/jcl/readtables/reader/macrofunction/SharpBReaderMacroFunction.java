package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.impl.macrofunctions.IntegerMacroFunctionReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#b' Lisp reader macro.
 */
public class SharpBReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		final IntegerMacroFunctionReader macroFunctionReader = new IntegerMacroFunctionReader(reader);
		return macroFunctionReader.readIntegerToken(2);
	}
}
