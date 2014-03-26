package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.FeaturesMacroFunctionReader;
import jcl.readtables.reader.LispReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '#-' Lisp reader macro.
 */
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final LispReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		final FeaturesMacroFunctionReader macroFunctionReader = new FeaturesMacroFunctionReader(reader);
		macroFunctionReader.readFeatures(true);
		return null;
	}
}
