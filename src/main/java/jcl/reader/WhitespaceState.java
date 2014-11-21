package jcl.reader;

/**
 * Step 3 of the Reader Algorithm.
 * <p>
 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
 */
public class WhitespaceState extends State {

	public static final State WHITESPACE_STATE = new WhitespaceState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		ReadState.READ_STATE.process(reader, tokenBuilder);
	}
}
