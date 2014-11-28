/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.AttributeType;
import jcl.reader.Reader;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Used to store a character token and its {@link AttributeType} value as a {@link Reader} process executes.
 */
public class TokenAttribute {

	/**
	 * The character token code point.
	 */
	private final int token;

	/**
	 * The {@link AttributeType} of the {@link #token}.
	 */
	private final AttributeType attributeType;

	/**
	 * Package private constructor.
	 *
	 * @param token
	 * 		the character token value
	 * @param attributeType
	 * 		the {@link AttributeType} of the character token
	 */
	TokenAttribute(final int token, final AttributeType attributeType) {
		this.token = token;
		this.attributeType = attributeType;
	}

	/**
	 * Getter for {@link #token} property.
	 *
	 * @return {@link #token} property
	 */
	public int getToken() {
		return token;
	}

	/**
	 * Getter for {@link #attributeType} property.
	 *
	 * @return {@link #attributeType} property
	 */
	public AttributeType getAttributeType() {
		return attributeType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
