package jcl.readtables.state;

/**
 * Step 3 of the Reader Algorithm.
 * <p/>
 * Discards all white space and returns the ReadState
 */
public class WhitespaceState implements State {

	public static final State WHITESPACE_STATE = new WhitespaceState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return ReadState
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);
		readerState.setNextState(ReadState.READ_STATE);
		return readerState;
	}
}
