package jcl.readtables.reader;

import jcl.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Step 2 of the Reader Algorithm.
 * <p/>
 * The IllegalCharacterState is reached only when an illegal character is reached.
 * <p/>
 */
public class IllegalCharacterState implements State {

	public static final State ILLEGAL_CHARACTER_STATE = new IllegalCharacterState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException thrown because we have found a character that is not defined within Unicode
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final Integer codePoint = readerState.getPreviousReadCharacter();
		if (codePoint == null) {
			readerState.setErrorMessage("End Of File Character was encountered.");
			readerState.setNextState(ErrorState.ERROR_STATE);
		} else if (codePoint == CharacterConstants.EXIT_CHAR) {
			readerState.setErrorMessage("Exit Character was encountered: " + CharacterConstants.EXIT_CHAR);
			readerState.setNextState(EndState.END_STATE);
		} else {
			readerState.setErrorMessage("Illegal Character was encountered: " + codePoint);
			readerState.setNextState(ErrorState.ERROR_STATE);
		}

		return readerState;
	}
}
