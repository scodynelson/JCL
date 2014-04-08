package jcl.readtables.reader.impl;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.syntax.TokenBuilder;
import jcl.syntax.CharacterConstants;

/**
 * Step 2 of the Reader Algorithm.
 * <p/>
 * The IllegalCharacterState is reached only when an illegal character is reached.
 * <p/>
 */
public class IllegalCharacterState extends State {

	public static final State ILLEGAL_CHARACTER_STATE = new IllegalCharacterState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException thrown because we have found a character that is not defined within Unicode
	 */
	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();
		if (codePoint == null) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.setErrorMessage("End Of File Character was encountered.");
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
		} else if (codePoint != CharacterConstants.EXIT_CHAR) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.setErrorMessage("Illegal Character was encountered: " + codePoint);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
		}
	}
}
