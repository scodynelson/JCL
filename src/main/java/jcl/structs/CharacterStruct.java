package jcl.structs;

import jcl.types.BaseChar;
import jcl.types.Character;
import jcl.types.ExtendedChar;
import jcl.types.StandardChar;
import org.apache.commons.lang3.CharUtils;

/**
 * The {@code CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public class CharacterStruct extends BuiltInClassStruct {

	private final int codePoint;

	/**
	 * Public constructor.
	 *
	 * @param codePoint the character code point value
	 */
	public CharacterStruct(final int codePoint) {
		super(getCharacterType(codePoint), null, null);
		this.codePoint = codePoint;
	}

	/**
	 * This method gets the character type from the provide character code point.
	 *
	 * @param codePoint the character code point value
	 * @return the matching character type for the provided character code point
	 */
	private static Character getCharacterType(final int codePoint) {
		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			return BaseChar.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			return StandardChar.INSTANCE;
		} else if (java.lang.Character.isDefined(codePoint)) {
			return ExtendedChar.INSTANCE;
		} else {
			return Character.INSTANCE;
		}
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
		return "CharacterStruct{"
				+ "codePoint=" + codePoint
				+ '}';
	}
}
