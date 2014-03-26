package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.macrofunctions.ExtendedTokenMacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.syntax.CharacterName;
import jcl.variables.ReadSuppressVariable;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#\' Lisp reader macro.
 */
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.BACKSLASH;

		final ExtendedTokenMacroFunctionReader macroFunctionReader = new ExtendedTokenMacroFunctionReader(reader);
		final String charString = macroFunctionReader.readExtendedTokenEscaped();
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
