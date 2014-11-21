package jcl.reader;

/**
 * Step 3 of the Reader Algorithm.
 * <p>
 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
 */
final class WhitespaceState extends State {

	static final State INSTANCE = new WhitespaceState();

	/**
	 * Private constructor.
	 */
	private WhitespaceState() {
	}

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		ReadState.INSTANCE.process(reader, tokenBuilder);
	}
}
