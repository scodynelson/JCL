package jcl.reader;

import jcl.LispStruct;
import jcl.structs.streams.ReadResult;

/**
 * Step 5 of the Reader Algorithm.
 * <p>
 * If x is a single escape character then the next character, y, is read, or an error of type end-of-file is signaled
 * if at the end of file. y is treated as if it is a constituent whose only constituent trait is alphabetic[2]. y is
 * used to begin a token, and step 8 is entered.
 * </p>
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
