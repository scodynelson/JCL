package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.state.StateReader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Not Specified in the Reader Algorithm.
 * <p/>
 * Begins the the transitions between States that are specified in the Reader Algorithm
 */
public class InitialState implements State {

	public static final State INITIAL_STATE = new InitialState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return ReadState    Returns the first State of the Reader Algorithm
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) throws ReaderErrorException {
		readerState.setPreviousState(this);
		readerState.setNextState(ReadState.READ_STATE);
		return readerState;
	}
}
