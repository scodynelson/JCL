package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.UnicodeCharacterReader;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.characters.CharacterStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the '#u' Lisp reader macro.
 */
public class SharpUReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_U) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U);

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final UnicodeCharacterReader macroFunctionReader = new UnicodeCharacterReader(reader);
		final int unicodeChar = macroFunctionReader.readUnicodeChar();
		return new CharacterStruct(unicodeChar);
	}
}
