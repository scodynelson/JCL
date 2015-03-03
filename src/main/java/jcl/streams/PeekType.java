/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Special object denoting a peek type in how the 'peek-char' should operate. There are 3 categories of PeekTypes:
 * <tab>
 * 1. {@link PeekType#NIL_PEEK_TYPE} - this denotes the NIL based PeekType
 * 2. {@link PeekType#T_PEEK_TYPE} - this denotes the T based PeekType
 * 3. Character based PeekTypes - these are created by calling the static method {@link PeekType#getCharacterPeekType}
 * to create a specific character-based PeekType based off of a provided {@link Integer} codePoint value
 * </tab>
 */
final class PeekType {

	/**
	 * Constant for 'T' PeekTypes.
	 */
	static final PeekType T_PEEK_TYPE = new PeekType(PeekTypeType.T, null);

	/**
	 * Constant for 'NIL' PeekTypes.
	 */
	static final PeekType NIL_PEEK_TYPE = new PeekType(PeekTypeType.NIL, null);

	/**
	 * The {@link PeekTypeType} of the peek type.
	 */
	private final PeekTypeType type;

	/**
	 * The {@link Integer} code point of the peek type when the peek type is a {@link PeekTypeType#CHARACTER}.
	 */
	private final Integer codePoint;

	/**
	 * Private constructor to create a PeekType with the provided {@link PeekTypeType} and {@link Integer} codePoint
	 * value.
	 *
	 * @param type
	 * 		the {@link PeekTypeType} value for the PeekType
	 * @param codePoint
	 * 		the {@link Integer} codePoint value for the PeekType
	 */
	private PeekType(final PeekTypeType type, final Integer codePoint) {
		this.type = type;
		this.codePoint = codePoint;
	}

	/**
	 * Getter for the {@link #type} value.
	 *
	 * @return the {@link #type} value
	 */
	PeekTypeType getType() {
		return type;
	}

	/**
	 * Getter for the {@link #codePoint} value.
	 *
	 * @return the {@link #codePoint} value
	 */
	Integer getCodePoint() {
		return codePoint;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * Gets a new {@link PeekTypeType#CHARACTER} based PeekType instance.
	 *
	 * @param codePoint
	 * 		the codePoint value of the character for the peek
	 *
	 * @return a new {@link PeekTypeType#CHARACTER} with the provided codePoint value
	 */
	static PeekType getCharacterPeekType(final int codePoint) {
		return new PeekType(PeekTypeType.CHARACTER, codePoint);
	}

	/**
	 * Package private enumeration to encapsulate the specific 'peek' type.
	 */
	enum PeekTypeType {

		/**
		 * T peek type.
		 */
		T,

		/**
		 * NIL peek type.
		 */
		NIL,

		/**
		 * Character peek type.
		 */
		CHARACTER
	}
}
