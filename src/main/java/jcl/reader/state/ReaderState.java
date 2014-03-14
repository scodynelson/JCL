package jcl.reader.state;

import jcl.reader.state.impl.InitialState;
import jcl.syntax.AttributeType;
import jcl.LispStruct;

import java.util.LinkedList;

public class ReaderState {

	private final LispStruct eofValue;
	private final boolean eofErrorP;
	private final boolean recursiveP;

	private LispStruct returnToken;
	private Integer previousReadCharacter;
	private String errorMessage;

	private State previousState;
	private State nextState;

	private final LinkedList<TokenAttribute> tokenAttributes;

	public ReaderState() {
		this(true, null, true);
	}

	public ReaderState(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		nextState = InitialState.INITIAL_STATE;
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

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(final String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public State getPreviousState() {
		return previousState;
	}

	public void setPreviousState(final State previousState) {
		this.previousState = previousState;
	}

	public State getNextState() {
		return nextState;
	}

	public void setNextState(final State nextState) {
		this.nextState = nextState;
	}

	public LinkedList<TokenAttribute> getTokenAttributes() {
		return new LinkedList<>(tokenAttributes);
	}

	public void addToTokenAttributes(final Integer token, final AttributeType attributeType) {
		final TokenAttribute tokenAttribute = new TokenAttribute(token, attributeType);
		tokenAttributes.add(tokenAttribute);
	}
}
