/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import jcl.lang.readtable.AttributeType;
import jcl.reader.Reader;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Used to store a character token and its {@link AttributeType} value as a {@link Reader} process executes.
 */
@Getter
@AllArgsConstructor
class TokenAttribute {

	/**
	 * The character token code point.
	 */
	private final int codePoint;

	/**
	 * The {@link AttributeType} of the {@link #codePoint}.
	 */
	private final AttributeType attributeType;
}
