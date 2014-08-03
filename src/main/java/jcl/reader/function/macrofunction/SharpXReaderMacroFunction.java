package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.RationalReader;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#x' Lisp reader macro.
 */
public class SharpXReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_X) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_X);

		final RationalReader macroFunctionReader = new RationalReader(reader);
		return macroFunctionReader.readRationalToken(16);
	}
}
