package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.state.StateReader;
import jcl.reader.syntax.AttributeType;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.ReadResult;

/**
 * Step 5 of the Reader Algorithm.
 * <p/>
 * SingleEscapeState will have already read in the single escape character prior to being executed.
 * This will read the character following the single escape character and put that value into the
 * tokenAccumulator, and that character will be treated as an ALPHABETIC character.
 * <p/>
 */
public class SingleEscapeState implements State {

	public static final State SINGLE_ESCAPE_STATE = new SingleEscapeState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return EvenMultiEscapeState  We have encountered 0 Multiple Escape Characters this far
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) throws ReaderErrorException {
		readerState.setPreviousState(this);

		final boolean isEofErrorP = readerState.isEofErrorP();
		final LispStruct eofValue = readerState.getEofValue();
		final boolean isRecursiveP = readerState.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			readerState.setNextState(IllegalCharacterState.ILLEGAL_CHARACTER_STATE);
			return readerState;
		}

		final int codePoint = readResult.getResult();
		readerState.setPreviousReadCharacter(codePoint);

		readerState.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

		readerState.setNextState(EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE);
		return readerState;
	}
}
