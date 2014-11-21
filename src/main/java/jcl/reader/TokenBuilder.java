package jcl.reader;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.LinkedList;

/**
 * Used to build {@link LispStruct} tokens as a {@link Reader} process executes.
 */
class TokenBuilder {

	private final boolean eofErrorP;
	private final LispStruct eofValue;
	private final boolean recursiveP;
	private final LinkedList<TokenAttribute> tokenAttributes;

	private LispStruct returnToken;
	private Integer previousReadCharacter;

	/**
	 * Package private constructor.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 */
	TokenBuilder(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		this.eofErrorP = eofErrorP;
		this.eofValue = eofValue;
		this.recursiveP = recursiveP;
		tokenAttributes = new LinkedList<>();

		returnToken = null;
		previousReadCharacter = null;
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
	 * Getter for {@link #recursiveP} property.
	 *
	 * @return {@link #recursiveP} property
	 */
	boolean isRecursiveP() {
		return recursiveP;
	}

	/**
	 * Getter for {@link #returnToken} property.
	 *
	 * @return {@link #returnToken} property
	 */
	LispStruct getReturnToken() {
		return returnToken;
	}

	/**
	 * Setter for the {@link #returnToken} property.
	 *
	 * @param returnToken
	 * 		the new value of the {@link #returnToken} property
	 */
	void setReturnToken(final LispStruct returnToken) {
		this.returnToken = returnToken;
	}

	/**
	 * Getter for {@link #previousReadCharacter} property.
	 *
	 * @return {@link #previousReadCharacter} property
	 */
	Integer getPreviousReadCharacter() {
		return previousReadCharacter;
	}

	/**
	 * Setter for the {@link #previousReadCharacter} property.
	 *
	 * @param previousReadCharacter
	 * 		the new value of the {@link #previousReadCharacter} property
	 */
	void setPreviousReadCharacter(final int previousReadCharacter) {
		this.previousReadCharacter = previousReadCharacter;
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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
