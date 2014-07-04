package jcl.readtables.reader.impl;

import jcl.LispStruct;
import jcl.readtables.reader.syntax.TokenBuilder;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

/**
 * Step 8 of the Reader Algorithm.
 * <p>
 * Character processing is done according to the HyperSpec.
 * <p>
 * Thus far we have reached 0,2,4... even Multiple Escape Characters.  The way it works is outlined
 * in the Reader algorithm.  The if statements have comments first so you can figure out what
 * each part of this code does.
 * <p>
 */
public class EvenMultiEscapeState extends State {

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
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

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

			codePoint = StateUtils.properCaseCodePoint(codePoint, attributeType, readtableCase);
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
