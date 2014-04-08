package jcl.readtables.reader.impl;

import jcl.readtables.reader.syntax.TokenBuilder;

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
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		ReadState.READ_STATE.process(reader, tokenBuilder);
	}
}
