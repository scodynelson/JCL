package jcl.reader.macrofunction;

import jcl.reader.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

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
