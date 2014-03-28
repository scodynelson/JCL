package jcl.readtables.reader.impl.state;

import jcl.readtables.reader.Reader;
import jcl.readtables.reader.impl.State;
import jcl.readtables.reader.syntax.TokenBuilder;

/**
 * Step 6 of the Reader Algorithm.
 */
public class MultipleEscapeState extends State {

	public static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
	}
}
