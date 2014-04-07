package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.function.RationalMacroFunctionReader;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#o' Lisp reader macro.
 */
public class SharpOReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_O) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_O);

		final RationalMacroFunctionReader macroFunctionReader = new RationalMacroFunctionReader(reader);
		return macroFunctionReader.readRationalToken(8);
	}
}
