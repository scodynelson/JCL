package jcl.readtables.state;

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
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);
		readerState.setNextState(ReadState.READ_STATE);
		return readerState;
	}
}
