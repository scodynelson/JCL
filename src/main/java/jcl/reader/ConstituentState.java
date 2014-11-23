package jcl.reader;

/**
 * Step 7 of the Reader Algorithm.
 * <p>
 * If x is a constituent character, then it begins a token. After the token is read in, it will be interpreted either
 * as a Lisp object or as being of invalid syntax. If the token represents an object, that object is returned as the
 * result of the read operation. If the token is of invalid syntax, an error is signaled. If x is a character with
 * case, it might be replaced with the corresponding character of the opposite case, depending on the readtable case of
 * the current readtable. X is used to begin a token, and step 8 is entered.
 * </p>
 */
class ConstituentState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new ConstituentState();

	/**
	 * Private constructor.
	 */
	private ConstituentState() {
	}

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (State.isEndOfFileCharacter(codePoint)) {
			State.handleEndOfFile(tokenBuilder, "ConstituentState");
			return;
		}

		final CaseSpec readtableCase = reader.getReadtableCase();
		final AttributeType attributeType = reader.getAttributeType(codePoint);

		codePoint = State.properCaseCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		EvenMultiEscapeState.INSTANCE.process(reader, tokenBuilder);
	}
}
