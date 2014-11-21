package jcl.reader;

import jcl.LispStruct;
import jcl.structs.streams.ReadResult;

/**
 * Step 8 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an even number of multiple escape characters have been encountered.
 * If at end of file, step 10 is entered. Otherwise, a character, y, is read, and one of the following actions is
 * performed according to its syntax type:
 * <tab>
 * <p>
 * If y is a constituent or non-terminating macro character:
 * <tab>
 * <p>
 * -- If y is a character with case, it might be replaced with the corresponding character of the opposite case,
 * depending on the readtable case of the current readtable.
 * </p>
 * <p>
 * -- Y is appended to the token being built.
 * </p>
 * <p>
 * -- Step 8 is repeated.
 * </p>
 * </tab>
 * </p>
 * <p>
 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
 * if at end of file. Z is treated as if it is a constituent whose only constituent trait is alphabetic. Z is appended
 * to the token being built, and step 8 is repeated.
 * </p>
 * <p>
 * If y is a multiple escape character, then step 9 is entered.
 * </p>
 * <p>
 * If y is an invalid character, an error of type reader-error is signaled.
 * </p>
 * <p>
 * If y is a terminating macro character, then it terminates the token. First the character y is unread, and then step
 * 10 is entered.
 * </p>
 * <p>
 * If y is a whitespace character, then it terminates the token. First the character y is unread if appropriate, and
 * then step 10 is entered.
 * </p>
 * </tab>
 * </p>
 */
final class EvenMultiEscapeState extends State {

	static final State EVEN_MULTI_ESCAPE_STATE = new EvenMultiEscapeState();

	/**
	 * Private constructor.
	 */
	private EvenMultiEscapeState() {
	}

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			TokenAccumulatedState.TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		}

		int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final SyntaxType syntaxType = reader.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			final CaseSpec readtableCase = reader.getReadtableCase();
			final AttributeType attributeType = reader.getAttributeType(codePoint);

			codePoint = properCaseCodePoint(codePoint, attributeType, readtableCase);
			tokenBuilder.addToTokenAttributes(codePoint, attributeType);

			EVEN_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			// NOTE: The only difference in the following logic and the actual SINGLE_ESCAPE_STATE is that
			//          this one builds on the current token, where as the SES begins a token.

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.wasEOF()) {
				IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
			} else {
				codePoint = readResult.getResult();
				tokenBuilder.setPreviousReadCharacter(codePoint);
				tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

				EVEN_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
			}
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.TERMINATING) {
			// TODO: preserve whitespace?
			reader.unreadChar(codePoint);
			TokenAccumulatedState.TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.WHITESPACE) {
			reader.unreadChar(codePoint);
			TokenAccumulatedState.TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		} else {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
		}
	}
}
