package jcl.readtables.reader;

/**
 * Step 3 of the Reader Algorithm.
 * <p/>
 * Discards all white space and returns the ReadState
 */
public class WhitespaceState extends State {

	public static final State WHITESPACE_STATE = new WhitespaceState();

	/**
	 * Processes for the reader for the current State.
	 */
	@Override
	public void process(final StateReader reader, final ReaderState readerState) {
		ReadState.READ_STATE.process(reader, readerState);
	}
}
