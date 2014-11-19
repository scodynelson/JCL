package jcl.reader;

import jcl.LispStruct;
import jcl.reader.syntax.SyntaxType;
import jcl.reader.syntax.TokenBuilder;
import jcl.structs.streams.ReadResult;

/**
 * Step 1 of the Reader Algorithm.
 * <p>
 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
 * </p>
 */
public class ReadState extends State {

	public static final State READ_STATE = new ReadState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			tokenBuilder.setReturnToken(null);

			final ErrorState errorState = new ErrorState(this, null);
			errorState.process(reader, tokenBuilder);
			return;
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final SyntaxType syntaxType = reader.getSyntaxType(codePoint);

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
