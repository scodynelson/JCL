/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters;

import jcl.classes.BuiltInClassStruct;
import jcl.types.BaseChar;
import jcl.types.ExtendedChar;
import jcl.types.StandardChar;
import org.apache.commons.lang3.CharUtils;

/**
 * The {@link CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public class CharacterStruct extends BuiltInClassStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -384956123492937850L;

	/**
	 * The code point of the character.
	 */
	private final int codePoint;

	/**
	 * Public constructor.
	 *
	 * @param codePoint
	 * 		the character {@link #codePoint} value
	 */
	public CharacterStruct(final int codePoint) {
		super(getCharacterType(codePoint), null, null);
		this.codePoint = codePoint;
	}

	/**
	 * This method gets the character type from the provide character {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character {@link #codePoint} value
	 *
	 * @return the matching character type for the provided character {@code codePoint}
	 */
	private static jcl.types.Character getCharacterType(final int codePoint) {
		final jcl.types.Character characterType;

		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			characterType = BaseChar.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			characterType = StandardChar.INSTANCE;
		} else if (Character.isDefined(codePoint)) {
			characterType = ExtendedChar.INSTANCE;
		} else {
			characterType = jcl.types.Character.INSTANCE;
		}

		return characterType;
	}

	/**
	 * Getter for character {@link #codePoint} property.
	 *
	 * @return character {@link #codePoint} property
	 */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Returns the character {@link #codePoint} property as a {@code char}.
	 *
	 * @return the character {@link #codePoint} property as a {@code char}
	 */
	public char getCharacter() {
		return (char) codePoint;
	}
}
