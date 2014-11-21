package jcl.reader;

import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Step 2 of the Reader Algorithm.
 * <p>
 * If x is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
final class IllegalCharacterState extends State {

	static final State INSTANCE = new IllegalCharacterState();

	/**
	 * Private constructor.
	 */
	private IllegalCharacterState() {
	}

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (isEndOfFileCharacter(codePoint) && tokenBuilder.isEofErrorP()) {
			throw new ReaderErrorException("End-of-File encountered in State: " + this);
		} else if (codePoint != CharacterConstants.EXIT_CHAR) {
			throw new ReaderErrorException("Illegal Character was encountered: " + codePoint);
		}
	}
}
