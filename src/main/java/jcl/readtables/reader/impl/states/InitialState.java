package jcl.readtables.reader.impl.states;

import jcl.readtables.reader.impl.TokenBuilder;

/**
 * Not Specified in the Reader Algorithm.
 * <p/>
 * Begins the the transitions between States that are specified in the Reader Algorithm
 */
public class InitialState extends State {

	public static final State INITIAL_STATE = new InitialState();

	@Override
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {
		ReadState.READ_STATE.process(reader, tokenBuilder);
	}
}
