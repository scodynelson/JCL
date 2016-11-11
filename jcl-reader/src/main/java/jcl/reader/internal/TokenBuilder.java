/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.internal;

import java.util.LinkedList;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.readtable.AttributeType;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.Reader;

/**
 * Used to build {@link LispStruct} tokens as a {@link Reader} process executes.
 */
class TokenBuilder {

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
	private final LinkedList<TokenAttribute> tokenAttributes;

	/**
	 * The previously read result. This value is null if no tokens have been read.
	 */
	private ReadPeekResult previousReadResult;

	/**
	 * Notes whether the token was multi-escaped.
	 */
	private boolean isMultiEscapedToken;

	/**
	 * Package private constructor.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read tokens from
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 */
	TokenBuilder(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue) {
		this.inputStreamStruct = inputStreamStruct;
		this.eofErrorP = eofErrorP;
		this.eofValue = eofValue;
		tokenAttributes = new LinkedList<>();

		previousReadResult = null;
		isMultiEscapedToken = false;
	}

	/**
	 * Getter for {@link #inputStreamStruct} property.
	 *
	 * @return {@link #inputStreamStruct} property
	 */
	InputStreamStruct getInputStreamStruct() {
		return inputStreamStruct;
	}

	/**
	 * Getter for {@link #eofErrorP} property.
	 *
	 * @return {@link #eofErrorP} property
	 */
	boolean isEofErrorP() {
		return eofErrorP;
	}

	/**
	 * Getter for {@link #eofValue} property.
	 *
	 * @return {@link #eofValue} property
	 */
	LispStruct getEofValue() {
		return eofValue;
	}

	/**
	 * Getter for {@link #tokenAttributes} property.
	 *
	 * @return {@link #tokenAttributes} property
	 */
	LinkedList<TokenAttribute> getTokenAttributes() {
		return tokenAttributes;
	}

	/**
	 * Creates a new {@link TokenAttribute} with the provided {@code token} and {@link AttributeType} and adds it to
	 * the {@link #tokenAttributes} list.
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
	 * Getter for {@link #previousReadResult} property.
	 *
	 * @return {@link #previousReadResult} property
	 */
	ReadPeekResult getPreviousReadResult() {
		return previousReadResult;
	}

	/**
	 * Setter for the {@link #previousReadResult} property.
	 *
	 * @param previousReadResult
	 * 		the new value of the {@link #previousReadResult} property
	 */
	void setPreviousReadResult(final ReadPeekResult previousReadResult) {
		this.previousReadResult = previousReadResult;
	}

	/**
	 * Getter for {@link #isMultiEscapedToken} property.
	 *
	 * @return {@link #isMultiEscapedToken} property
	 */
	boolean isMultiEscapedToken() {
		return isMultiEscapedToken;
	}

	/**
	 * Setter for the {@link #isMultiEscapedToken} property that will always set the value to true.
	 */
	void setMultiEscapedToken() {
		isMultiEscapedToken = true;
	}
}