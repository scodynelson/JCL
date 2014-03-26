package jcl.readtables.reader.impl.states.impl;

import jcl.LispStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.impl.states.TokenBuilder;
import jcl.syntax.AttributeType;
import jcl.syntax.reader.ReadResult;

/**
 * Step 5 of the Reader Algorithm.
 * <p/>
 * SingleEscapeState will have already read in the single escape character prior to being executed.
 * This will read the character following the single escape character and put that value into the
 * tokenAccumulator, and that character will be treated as an ALPHABETIC character.
 * <p/>
 */
public class SingleEscapeState extends State {

	public static final State SINGLE_ESCAPE_STATE = new SingleEscapeState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, tokenBuilder);
		} else {
			final int codePoint = readResult.getResult();
			tokenBuilder.setPreviousReadCharacter(codePoint);

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
		}
	}
}
