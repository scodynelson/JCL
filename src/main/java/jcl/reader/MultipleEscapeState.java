package jcl.reader;

import jcl.reader.syntax.TokenBuilder;

/**
 * Step 6 of the Reader Algorithm.
 * <p>
 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
 * entered.
 * </p>
 */
public class MultipleEscapeState extends State {

	public static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
	}
}
