package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.FeaturesMacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#+' Lisp reader macro.
 */
public class SharpPlusSignReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.PLUS_SIGN;

		final FeaturesMacroFunctionReader macroFunctionReader = new FeaturesMacroFunctionReader(reader);
		macroFunctionReader.readFeatures(false);
		return null;
	}
}
