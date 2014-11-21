package jcl.reader;

import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Step 2 of the Reader Algorithm.
 * <p>
 * If x is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
public class IllegalCharacterState extends State {

	public static final State ILLEGAL_CHARACTER_STATE = new IllegalCharacterState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (isEndOfFileCharacter(codePoint) && tokenBuilder.isEofErrorP()) {
			throw new ReaderErrorException("End-of-File encountered in State: " + this);
		} else if (codePoint != CharacterConstants.EXIT_CHAR) {
			throw new ReaderErrorException("Illegal Character was encountered: " + codePoint);
		}
	}
}
