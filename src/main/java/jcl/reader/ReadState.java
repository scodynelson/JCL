package jcl.reader;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.ReadResult;

/**
 * Step 1 of the Reader Algorithm.
 * <p>
 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
 * </p>
 */
final class ReadState extends State {

	static final State INSTANCE = new ReadState();

	/**
	 * Private constructor.
	 */
	private ReadState() {
	}

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			if (tokenBuilder.isEofErrorP()) {
				throw new ReaderErrorException("End-of-File encountered in State: " + this);
			} else {
				tokenBuilder.setReturnToken(null);
				return;
			}
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final SyntaxType syntaxType = reader.getSyntaxType(codePoint);

		if (syntaxType == SyntaxType.WHITESPACE) {
			WhitespaceState.INSTANCE.process(reader, tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			MacroCharacterState.INSTANCE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			SingleEscapeState.INSTANCE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			MultipleEscapeState.INSTANCE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			ConstituentState.INSTANCE.process(reader, tokenBuilder);
		} else {
			IllegalCharacterState.INSTANCE.process(reader, tokenBuilder);
		}
	}
}
