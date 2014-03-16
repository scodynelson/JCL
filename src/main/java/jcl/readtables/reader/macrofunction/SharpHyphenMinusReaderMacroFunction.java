package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.MacroFunctionReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#-' Lisp reader macro.
 */
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		reader.readFeatures(true);
		return null;
	}
}
