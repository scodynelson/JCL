package jcl.readtables.reader.impl.states;

import jcl.LispStruct;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.StateReader;
import jcl.readtables.reader.impl.TokenBuilder;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

/**
 * Step 1 of the Reader Algorithm.
 * <p/>
 * The ReadState will read a character in from the input Stream and then go to the
 * next proper State according to what the SyntaxType of the character that was read
 * from the input stream was.
 */
public class ReadState extends State {

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
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			tokenBuilder.setReturnToken(null);

			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = reader.getReadtable();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if (syntaxType == SyntaxType.WHITESPACE) {
			WhitespaceState.WHITESPACE_STATE.process(reader, tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			MacroCharacterState.MACRO_CHARACTER_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			SingleEscapeState.SINGLE_ESCAPE_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			MultipleEscapeState.MULTIPLE_ESCAPE_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			ConstituentState.CONSTITUENT_STATE.process(reader, tokenBuilder);
		} else {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
		}
	}
}
