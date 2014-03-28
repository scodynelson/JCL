package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.functionreader.IntegerMacroFunctionReader;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#x' Lisp reader macro.
 */
public class SharpXReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_X) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_X);

		final IntegerMacroFunctionReader macroFunctionReader = new IntegerMacroFunctionReader(reader);
		return macroFunctionReader.readIntegerToken(16);
	}
}
