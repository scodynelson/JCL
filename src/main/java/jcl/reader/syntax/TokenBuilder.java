package jcl.reader.syntax;

import jcl.LispStruct;

import java.util.LinkedList;

public class TokenBuilder {

	private final LispStruct eofValue;
	private final boolean eofErrorP;
	private final boolean recursiveP;
	private final LinkedList<TokenAttribute> tokenAttributes;
	private LispStruct returnToken;
	private Integer previousReadCharacter;

	public TokenBuilder() {
		this(true, null, true);
	}

	public TokenBuilder(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		this.eofErrorP = eofErrorP;
		this.eofValue = eofValue;
		this.recursiveP = recursiveP;
		tokenAttributes = new LinkedList<>();
	}

	public LispStruct getEofValue() {
		return eofValue;
	}

	public boolean isEofErrorP() {
		return eofErrorP;
	}

	public boolean isRecursiveP() {
		return recursiveP;
	}

	public LispStruct getReturnToken() {
		return returnToken;
	}

	public void setReturnToken(final LispStruct returnToken) {
		this.returnToken = returnToken;
	}

	public Integer getPreviousReadCharacter() {
		return previousReadCharacter;
	}

	public void setPreviousReadCharacter(final int previousReadCharacter) {
		this.previousReadCharacter = previousReadCharacter;
	}

	public LinkedList<TokenAttribute> getTokenAttributes() {
		return new LinkedList<>(tokenAttributes);
	}

	public void addToTokenAttributes(final Integer token, final AttributeType attributeType) {
		final TokenAttribute tokenAttribute = new TokenAttribute(token, attributeType);
		tokenAttributes.add(tokenAttribute);
	}
}
