package jcl.structs.characters;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.characters.BaseChar;
import jcl.types.characters.Character;
import jcl.types.characters.ExtendedChar;
import jcl.types.characters.StandardChar;
import org.apache.commons.lang3.CharUtils;

/**
 * The {@code CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public class CharacterStruct implements LispStruct {

	private final int codePoint;
	private final Character characterType;

	/**
	 * Private constructor.
	 *
	 * @param codePoint the character code point value
	 */
	private CharacterStruct(final int codePoint) {
		this.codePoint = codePoint;

		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			characterType = BaseChar.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			characterType = StandardChar.INSTANCE;
		} else if (java.lang.Character.isDefined(codePoint)) {
			characterType = ExtendedChar.INSTANCE;
		} else {
			characterType = Character.INSTANCE;
		}
	}

	@Override
	public LispType getType() {
		return characterType;
	}

	/**
	 * Method to retrieve the character code point.
	 *
	 * @return the character code point
	 */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Method to retrieve the character code point as a {@code char}.
	 *
	 * @return the character code point as a {@code char}
	 */
	public char getCharacter() {
		return (char) codePoint;
	}

	@Override
	public String toString() {
		return "CharacterStruct{" +
				"codePoint=" + codePoint +
				", characterType=" + characterType +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code CharacterStruct} for the provided {@code codePoint}.
	 *
	 * @param codePoint the character code point value
	 * @return the created {@code CharacterStruct}
	 */
	public static CharacterStruct getStruct(final int codePoint) {
		return new CharacterStruct(codePoint);
	}
}
