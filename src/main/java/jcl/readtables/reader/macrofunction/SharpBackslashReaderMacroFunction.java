package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.functionreader.ExtendedTokenMacroFunctionReader;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.syntax.CharacterName;
import jcl.readtables.reader.syntax.ReadExtendedToken;
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

		final ExtendedTokenMacroFunctionReader macroFunctionReader = new ExtendedTokenMacroFunctionReader(reader);
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
