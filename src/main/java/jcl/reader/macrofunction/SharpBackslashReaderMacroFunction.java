package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.syntax.CharacterName;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.structs.characters.CharacterStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.StringUtils;

import java.math.BigInteger;

/**
 * Implements the '#\' Lisp reader macro.
 */
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.BACKSLASH;

		final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader, true);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.process();
		final String charString = readExtendedToken.getToken();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (StringUtils.length(charString) == 1) {
			final char theChar = charString.charAt(0);
			return new CharacterStruct(theChar);
		}

		Character charName = null;
		for (final CharacterName characterName : CharacterName.values()) {
			final String name = characterName.getName();
			if (StringUtils.equalsIgnoreCase(charString, name)) {
				charName = characterName.getChar();
				break;
			}
		}

		if (charName == null) {
			throw new ReaderErrorException("Unrecognized character name: " + charString);
		}

		return new CharacterStruct(charName);
	}
}
