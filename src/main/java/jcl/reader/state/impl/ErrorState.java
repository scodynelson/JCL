package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.StateReader;
import jcl.reader.ReaderUtils;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Not Specified in the Reader Algorithm.
 * <p/>
 * This state should never be processed.  If it does, then a programming error has occurred.
 * <p/>
 * This is our Accepting state that says we are done with the Reader.
 */
public class ErrorState implements State {

	public static final State ERROR_STATE = new ErrorState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException thrown if the process method ever gets called. This can only be done explicitly by a programmer.
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {

		final Integer codePoint = readerState.getPreviousReadCharacter();
		if (ReaderUtils.isEndOfFileCharacter(codePoint) && !readerState.isEofErrorP()) {
			readerState.setNextState(EndState.END_STATE);
			return readerState;
		}
		throw new ReaderErrorException("Reader Error " + readerState.getErrorMessage() + " in State " + readerState.getPreviousState());
	}
}
