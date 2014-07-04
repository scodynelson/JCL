package jcl.readtables.reader.impl;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.syntax.TokenBuilder;

/**
 * Not Specified in the Reader Algorithm.
 * <p>
 * This state should never be processed.  If it does, then a programming error has occurred.
 * <p>
 * This is our Accepting state that says we are done with the Reader.
 */
public class ErrorState extends State {

	public static final ErrorState ERROR_STATE = new ErrorState();

	private State previousState;
	private String errorMessage;

	public void setPreviousState(final State previousState) {
		this.previousState = previousState;
	}

	public void setErrorMessage(final String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException thrown if the process method ever gets called. This can only be done explicitly by a programmer.
	 */
	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();
		if (StateUtils.isEndOfFileCharacter(codePoint) && !tokenBuilder.isEofErrorP()) {
			return;
		}
		throw new ReaderErrorException("Reader Error " + errorMessage + " in State " + previousState);
	}
}
