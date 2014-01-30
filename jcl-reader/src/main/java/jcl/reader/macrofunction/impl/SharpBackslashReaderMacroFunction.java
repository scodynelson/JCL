package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.syntax.CharacterName;
import jcl.structs.LispStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.types.Variable;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#\' Lisp reader macro.
 */
public class SharpBackslashReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert codePoint == CharacterConstants.BACKSLASH;

		final String charString = reader.readExtendedTokenEscaped();

		if (Variable.ReadSuppress) {
			return null;
		}

		if (StringUtils.length(charString) == 1) {
			final char theChar = charString.charAt(0);
			return CharacterStruct.getStruct(theChar);
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

		return CharacterStruct.getStruct(charName);
	}
}
