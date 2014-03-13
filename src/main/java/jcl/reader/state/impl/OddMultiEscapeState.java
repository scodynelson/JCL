package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.state.StateReader;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.LispStruct;
import jcl.structs.ReadtableStruct;
import jcl.structs.streams.ReadResult;

/**
 * Step 9 of the Reader Algorithm.
 * <p/>
 * Character processing is done according to the HyperSpec.
 * <p/>
 * Thus far we have reached 0,2,4... even Multiple Escape Characters.  The way it works is outlined
 * in the Reader Algorithm.  The if statements are commented first so you can figure out what
 * each part of this code does.
 * <p/>
 */
public class OddMultiEscapeState implements State {

	public static final State ODD_MULTI_ESCAPE_STATE = new OddMultiEscapeState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return TokenAccumulatedState    if we have reached the End Of File marker
	 * EvenMultiEscapeState     if we have found another Multiple Escape Character, toggle to OddMultiEscapeState
	 * OddMultiEscapeState      if we have found either a constituent character, non-terminating macro character,
	 * terminating macro character, whitespace character, or a single escape character
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final boolean isEofErrorP = readerState.isEofErrorP();
		final LispStruct eofValue = readerState.getEofValue();
		final boolean isRecursiveP = readerState.isRecursiveP();

		ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			readerState.setNextState(IllegalCharacterState.ILLEGAL_CHARACTER_STATE);
			return readerState;
		}

		int codePoint = readResult.getResult();
		readerState.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = reader.getReadtable();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		final State nextState;
		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			readerState.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			nextState = ODD_MULTI_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.wasEOF()) {
				readerState.setNextState(IllegalCharacterState.ILLEGAL_CHARACTER_STATE);
				return readerState;
			}

			codePoint = readResult.getResult();
			readerState.setPreviousReadCharacter(codePoint);
			readerState.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			nextState = ODD_MULTI_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			nextState = EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE;
		} else {
			nextState = IllegalCharacterState.ILLEGAL_CHARACTER_STATE;
		}

		readerState.setNextState(nextState);
		return readerState;
	}
}
