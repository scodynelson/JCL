package jcl.reader;

import jcl.reader.syntax.AttributeType;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Used to store a character token and its {@link AttributeType} value as a {@link Reader} process executes.
 */
class TokenAttribute {

	private final int token;
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
	int getToken() {
		return token;
	}

	/**
	 * Getter for {@link #attributeType} property.
	 *
	 * @return {@link #attributeType} property
	 */
	AttributeType getAttributeType() {
		return attributeType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
