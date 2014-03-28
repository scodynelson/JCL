package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.readtables.reader.functionreader.UnicodeCharMacroFunctionReader;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

/**
 * Implements the '#u' Lisp reader macro.
 */
public class SharpUReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_U) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U);

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		final UnicodeCharMacroFunctionReader macroFunctionReader = new UnicodeCharMacroFunctionReader(reader);
		final int unicodeChar = macroFunctionReader.readUnicodeChar();
		return new CharacterStruct(unicodeChar);
	}
}
