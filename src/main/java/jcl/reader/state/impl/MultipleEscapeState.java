package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.StateReader;

/**
 * Step 6 of the Reader Algorithm.
 */
public class MultipleEscapeState implements State {

	public static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return OddMultiEscapeState
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);
		readerState.setNextState(OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE);
		return readerState;
	}
}
