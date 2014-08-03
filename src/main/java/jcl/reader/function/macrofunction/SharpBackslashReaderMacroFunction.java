package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.reader.function.ExtendedTokenReader;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.CharacterName;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#\' Lisp reader macro.
 */
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.BACKSLASH;

		final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.readExtendedToken(true);
		final String charString = readExtendedToken.getToken();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
