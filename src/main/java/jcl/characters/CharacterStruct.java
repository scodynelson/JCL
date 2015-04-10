/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters;

import jcl.classes.BuiltInClassStruct;
import jcl.types.BaseCharType;
import jcl.types.CharacterType;
import jcl.types.ExtendedCharType;
import jcl.types.StandardCharType;
import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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
	private static CharacterType getCharacterType(final int codePoint) {
		final CharacterType characterType;

		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			characterType = BaseCharType.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			characterType = StandardCharType.INSTANCE;
		} else if (Character.isDefined(codePoint)) {
			characterType = ExtendedCharType.INSTANCE;
		} else {
			characterType = CharacterType.INSTANCE;
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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(codePoint)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final CharacterStruct rhs = (CharacterStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(codePoint, rhs.codePoint)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(codePoint)
		                                                                .toString();
	}
}
