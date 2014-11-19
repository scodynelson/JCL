package jcl.reader;

import jcl.reader.syntax.TokenBuilder;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Not Specified in the Reader Algorithm.
 * <p>
 * This state should never be processed. If it does, then a programming error has occurred.
 * </p>
 * <p>
 * This is our Accepting state that says we are done with the Reader.
 * </p>
 */
public class ErrorState extends State {

	private final State previousState;
	private final String errorMessage;

	/**
	 * Public constructor noting an error state during reader processing.
	 *
	 * @param previousState
	 * 		the previous state that failed
	 * @param errorMessage
	 * 		the message to throw
	 */
	public ErrorState(final State previousState, final String errorMessage) {
		this.previousState = previousState;
		this.errorMessage = errorMessage;
	}

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException
	 * 		thrown if the process method ever gets called. This can only be done explicitly by a programmer.
	 */
	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();
		if (isEndOfFileCharacter(codePoint) && !tokenBuilder.isEofErrorP()) {
			return;
		}
		throw new ReaderErrorException("Reader Error " + errorMessage + " in State " + previousState);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
