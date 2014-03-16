package jcl.readtables.reader.impl.states;

import jcl.readtables.reader.StateReader;
import jcl.readtables.reader.impl.TokenBuilder;

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
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {
		ReadState.READ_STATE.process(reader, tokenBuilder);
	}
}
