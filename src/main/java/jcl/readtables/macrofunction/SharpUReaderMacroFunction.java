package jcl.readtables.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.readtables.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#u' Lisp reader macro.
 */
public class SharpUReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_U) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U);

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		final int unicodeChar = reader.readUnicodeChar();
		return new CharacterStruct(unicodeChar);
	}
}
