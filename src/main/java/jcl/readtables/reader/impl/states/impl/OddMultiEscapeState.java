package jcl.readtables.reader.impl.states.impl;

import jcl.LispStruct;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.impl.states.State;
import jcl.readtables.reader.impl.states.TokenBuilder;
import jcl.syntax.AttributeType;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

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
public class OddMultiEscapeState extends State {

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
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
		}

		int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final ReadtableStruct readtable = reader.getReadtable();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.wasEOF()) {
				IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
			} else {
				codePoint = readResult.getResult();
				tokenBuilder.setPreviousReadCharacter(codePoint);
				tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

				ODD_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
			}
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
		} else {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
		}
	}
}
