/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import java.util.LinkedList;

import jcl.lang.AttributeType;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadCharResult;
import jcl.reader.Reader;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;

/**
 * Used to build {@link LispStruct} tokens as a {@link Reader} process executes.
 */
@Getter
@RequiredArgsConstructor
final class TokenBuilder {

	/**
	 * The {@link InputStreamStruct} to use when building lisp tokens.
	 */
	private final InputStreamStruct inputStreamStruct;

	/**
	 * Determines if an error should be thrown when an End-of-File character is encountered.
	 */
	private final boolean eofErrorP;

	/**
	 * The value to return if an End-of-File character is encountered and {@link #eofErrorP} is false.
	 */
	private final LispStruct eofValue;

	/**
	 * The current list of {@link TokenAttribute}s accumulated through the read operation.
	 */
	private final LinkedList<TokenAttribute> tokenAttributes = new LinkedList<>();

	/**
	 * The previously read result. This value is null if no tokens have been read.
	 */
	@Setter
	private ReadCharResult previousReadResult;

	/**
	 * Notes whether the token was multi-escaped.
	 */
	private boolean isMultiEscapedToken;

	/**
	 * Creates a new {@link TokenAttribute} with the provided {@code token} and {@link AttributeType} and adds it to the
	 * {@link #tokenAttributes} list.
	 *
	 * @param token
	 * 		the token to add
	 * @param attributeType
	 * 		the {@link AttributeType} of the token to add
	 */
	void addToTokenAttributes(final int token, final AttributeType attributeType) {
		final TokenAttribute tokenAttribute = new TokenAttribute(token, attributeType);
		tokenAttributes.add(tokenAttribute);
	}

	/**
	 * Setter for the {@link #isMultiEscapedToken} property that will always set the value to true.
	 */
	void setMultiEscapedToken() {
		isMultiEscapedToken = true;
	}
}
