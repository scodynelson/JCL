package jcl.reader;

/**
 * Step 6 of the Reader Algorithm.
 * <p>
 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
 * entered.
 * </p>
 */
final class MultipleEscapeState extends State {

	static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	/**
	 * Private constructor.
	 */
	private MultipleEscapeState() {
	}

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
	}
}
