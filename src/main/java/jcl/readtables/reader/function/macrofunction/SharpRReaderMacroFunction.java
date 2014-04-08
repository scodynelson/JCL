package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.function.RationalReader;
import jcl.readtables.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#r' Lisp reader macro.
 */
public class SharpRReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		final RationalReader macroFunctionReader = new RationalReader(reader);
		return macroFunctionReader.readRationalToken(numArg);
	}
}
