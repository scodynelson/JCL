package jcl.reader.macrofunction;

import jcl.reader.MacroFunctionReader;
import jcl.reader.ReaderMacroFunction;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

/**
 * Implements the '#+' Lisp reader macro.
 */
public class SharpPlusSignReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.PLUS_SIGN;

		reader.readFeatures(false);
		return null;
	}
}
