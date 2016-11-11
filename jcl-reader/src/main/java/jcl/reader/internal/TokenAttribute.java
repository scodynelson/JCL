/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import jcl.lang.readtable.AttributeType;
import jcl.reader.Reader;

/**
 * Used to store a character token and its {@link AttributeType} value as a {@link Reader} process executes.
 */
public class TokenAttribute {

	/**
	 * The character token code point.
	 */
	private final int codePoint;

	/**
	 * The {@link AttributeType} of the {@link #codePoint}.
	 */
	private final AttributeType attributeType;

	/**
	 * Package private constructor.
	 *
	 * @param codePoint
	 * 		the character token code point
	 * @param attributeType
	 * 		the {@link AttributeType} of the character token
	 */
	TokenAttribute(final int codePoint, final AttributeType attributeType) {
		this.codePoint = codePoint;
		this.attributeType = attributeType;
	}

	/**
	 * Getter for {@link #codePoint} property.
	 *
	 * @return {@link #codePoint} property
	 */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Getter for {@link #attributeType} property.
	 *
	 * @return {@link #attributeType} property
	 */
	public AttributeType getAttributeType() {
		return attributeType;
	}
}
