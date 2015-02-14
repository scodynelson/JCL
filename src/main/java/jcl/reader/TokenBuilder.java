/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.LinkedList;

/**
 * Used to build {@link LispStruct} tokens as a {@link Reader} process executes.
 */
public class TokenBuilder {

	/**
	 * The {@link Reader} instance to use when building lisp tokens.
	 */
	private final Reader reader;

	/**
	 * Determines if an error should be thrown when an End-of-File character is encountered.
	 */
	private final boolean eofErrorP;

	/**
	 * The value to return if an End-of-File character is encountered and {@link #eofErrorP} is false.
	 */
	private final LispStruct eofValue;

	/**
	 * Determines if the read operation should operate recursively for lisp tokens.
	 */
	private final boolean recursiveP;

	/**
	 * The current list of {@link TokenAttribute}s accumulated through the read operation.
	 */
	private final LinkedList<TokenAttribute> tokenAttributes;

	/**
	 * The previously read result. This value is null if no tokens have been read.
	 */
	private ReadPeekResult previousReadResult;

	/**
	 * Package private constructor.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read tokens
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 */
	TokenBuilder(final Reader reader, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		this.reader = reader;
		this.eofErrorP = eofErrorP;
		this.eofValue = eofValue;
		this.recursiveP = recursiveP;
		tokenAttributes = new LinkedList<>();

		previousReadResult = null;
	}

	/**
	 * Getter for {@link #reader} property.
	 *
	 * @return {@link #reader} property
	 */
	public Reader getReader() {
		return reader;
	}

	/**
	 * Getter for {@link #eofErrorP} property.
	 *
	 * @return {@link #eofErrorP} property
	 */
	public boolean isEofErrorP() {
		return eofErrorP;
	}

	/**
	 * Getter for {@link #eofValue} property.
	 *
	 * @return {@link #eofValue} property
	 */
	public LispStruct getEofValue() {
		return eofValue;
	}

	/**
	 * Getter for {@link #recursiveP} property.
	 *
	 * @return {@link #recursiveP} property
	 */
	public boolean isRecursiveP() {
		return recursiveP;
	}

	/**
	 * Getter for {@link #previousReadResult} property.
	 *
	 * @return {@link #previousReadResult} property
	 */
	public ReadPeekResult getPreviousReadResult() {
		return previousReadResult;
	}

	/**
	 * Setter for the {@link #previousReadResult} property.
	 *
	 * @param previousReadResult
	 * 		the new value of the {@link #previousReadResult} property
	 */
	public void setPreviousReadResult(final ReadPeekResult previousReadResult) {
		this.previousReadResult = previousReadResult;
	}

	/**
	 * Getter for {@link #tokenAttributes} property.
	 *
	 * @return {@link #tokenAttributes} property
	 */
	public LinkedList<TokenAttribute> getTokenAttributes() {
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
	public void addToTokenAttributes(final int token, final AttributeType attributeType) {
		final TokenAttribute tokenAttribute = new TokenAttribute(token, attributeType);
		tokenAttributes.add(tokenAttribute);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
