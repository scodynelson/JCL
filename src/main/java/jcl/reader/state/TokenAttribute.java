package jcl.reader.state;

import jcl.syntax.AttributeType;

public class TokenAttribute {

	private final int token;
	private final AttributeType attributeType;

	public TokenAttribute(final int token, final AttributeType attributeType) {
		this.token = token;
		this.attributeType = attributeType;
	}

	public int getToken() {
		return token;
	}

	public AttributeType getAttributeType() {
		return attributeType;
	}
}
