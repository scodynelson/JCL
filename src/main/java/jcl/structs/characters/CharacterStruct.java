/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.characters;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.symbols.variables.Variable;
import jcl.types.BaseChar;
import jcl.types.Character;
import jcl.types.ExtendedChar;
import jcl.types.StandardChar;
import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public class CharacterStruct extends BuiltInClassStruct {

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
	private static Character getCharacterType(final int codePoint) {
		final Character characterType;

		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			characterType = BaseChar.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			characterType = StandardChar.INSTANCE;
		} else if (java.lang.Character.isDefined(codePoint)) {
			characterType = ExtendedChar.INSTANCE;
		} else {
			characterType = Character.INSTANCE;
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

	@Override
	public String printStruct() {
		final boolean printEscape = Variable.PRINT_ESCAPE.getValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append("#\\");
		}

		if (java.lang.Character.isLetterOrDigit(codePoint)) {
			stringBuilder.append(codePoint);
		} else {
			stringBuilder.append(java.lang.Character.getName(codePoint));
		}

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
