package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.StateReader;
import jcl.syntax.SyntaxType;
import jcl.LispStruct;
import jcl.structs.ReadtableStruct;
import jcl.syntax.reader.ReadResult;

/**
 * Step 1 of the Reader Algorithm.
 * <p/>
 * The ReadState will read a character in from the input Stream and then go to the
 * next proper State according to what the SyntaxType of the character that was read
 * from the input stream was.
 */
public class ReadState implements State {

	public static final State READ_STATE = new ReadState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return value within the corresponding list:
	 * <StartIf>
	 * <If_Constituent>         ConstituentState
	 * <If_WhiteSpace>          WhitespaceState
	 * <If_Terminating>         MacroCharacterState
	 * <If_Non_Terminating>     MacroCharacterState
	 * <If_Single_Escape>       SingleEscapeState
	 * <If_Multiple_Escape>     MultiEscapeState
	 * <Else>                   IllegalCharacterState
	 * <EndIf>
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final boolean isEofErrorP = readerState.isEofErrorP();
		final LispStruct eofValue = readerState.getEofValue();
		final boolean isRecursiveP = readerState.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			readerState.setReturnToken(null);
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}

		final int codePoint = readResult.getResult();
		readerState.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = reader.getReadtable();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		final State nextState;
		if (syntaxType == SyntaxType.WHITESPACE) {
			nextState = WhitespaceState.WHITESPACE_STATE;
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			nextState = MacroCharacterState.MACRO_CHARACTER_STATE;
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			nextState = SingleEscapeState.SINGLE_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			nextState = MultipleEscapeState.MULTIPLE_ESCAPE_STATE;
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			nextState = ConstituentState.CONSTITUENT_STATE;
		} else {
			nextState = IllegalCharacterState.ILLEGAL_CHARACTER_STATE;
		}

		readerState.setNextState(nextState);
		return readerState;
	}
}
