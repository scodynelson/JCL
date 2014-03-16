package jcl.readtables.reader;

import jcl.readtables.TokenBuilder;

/**
 * Step 6 of the Reader Algorithm.
 */
public class MultipleEscapeState extends State {

	public static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	@Override
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {
		OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
	}
}
