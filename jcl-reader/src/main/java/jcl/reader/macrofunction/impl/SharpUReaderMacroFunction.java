package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.types.Variable;

/**
 * Implements the '#u' Lisp reader macro.
 */
public class SharpUReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_U) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U);

		if (Variable.ReadSuppress) {
			return null;
		} else {
			final int unicodeChar = reader.readUnicodeChar();
			return new CharacterStruct(unicodeChar);
		}
	}
}
