package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.state.StateReader;
import jcl.reader.state.impl.util.StateUtils;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.SyntaxType;
import jcl.LispStruct;
import jcl.structs.ReadtableStruct;
import jcl.structs.streams.ReadResult;

/**
 * Step 8 of the Reader Algorithm.
 * <p/>
 * Character processing is done according to the HyperSpec.
 * <p/>
 * Thus far we have reached 0,2,4... even Multiple Escape Characters.  The way it works is outlined
 * in the Reader algorithm.  The if statements have comments first so you can figure out what
 * each part of this code does.
 * <p/>
 */
public class EvenMultiEscapeState implements State {

	public static final State EVEN_MULTI_ESCAPE_STATE = new EvenMultiEscapeState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return TokenAccumulatedState    if we have reached the End Of File marker or a whitespace character
	 * or a terminating macro character
	 * OddMultiEscapeState      if we have found another Multiple Escape Character, toggle to OddMultiEscapeState
	 * EvenMultiEscapeState     if we have found either a constituent character, non-terminating macro
	 * character, or a single escape character
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final boolean isEofErrorP = readerState.isEofErrorP();
		final LispStruct eofValue = readerState.getEofValue();
		final boolean isRecursiveP = readerState.isRecursiveP();

		ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			readerState.setNextState(TokenAccumulatedState.TOKEN_ACCUMULATED_STATE);
			return readerState;
		}

		int codePoint = readResult.getResult();
		readerState.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = reader.getReadtable();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		final State nextState;
		if ((syntaxType == SyntaxType.CONSTITUENT) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			final CaseSpec readtableCase = readtable.getReadtableCase();
			final AttributeType attributeType = readtable.getAttributeType(codePoint);

			codePoint = StateUtils.properCaseCodePoint(codePoint, attributeType, readtableCase);
			readerState.addToTokenAttributes(codePoint, attributeType);

			nextState = EVEN_MULTI_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			// NOTE: The only difference in the following logic and the actual SINGLE_ESCAPE_STATE is that
			//          this one builds on the current token, where as the SES begins a token.

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.wasEOF()) {
				readerState.setNextState(IllegalCharacterState.ILLEGAL_CHARACTER_STATE);
				return readerState;
			}

			codePoint = readResult.getResult();
			readerState.setPreviousReadCharacter(codePoint);
			readerState.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			nextState = EVEN_MULTI_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			nextState = OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.TERMINATING) {
			reader.unreadChar(codePoint);
			nextState = TokenAccumulatedState.TOKEN_ACCUMULATED_STATE;
		} else if (syntaxType == SyntaxType.WHITESPACE) {
			// TODO: We want to take "read-preserving-whitespace" into account here before unreading
			reader.unreadChar(codePoint);
			nextState = TokenAccumulatedState.TOKEN_ACCUMULATED_STATE;
		} else {
			nextState = IllegalCharacterState.ILLEGAL_CHARACTER_STATE;
		}

		readerState.setNextState(nextState);
		return readerState;
	}
}
