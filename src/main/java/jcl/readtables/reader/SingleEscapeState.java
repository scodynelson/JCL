package jcl.readtables.reader;

import jcl.LispStruct;
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
	public void process(final StateReader reader, final ReaderState readerState) {
		final boolean isEofErrorP = readerState.isEofErrorP();
		final LispStruct eofValue = readerState.getEofValue();
		final boolean isRecursiveP = readerState.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			IllegalCharacterState.ILLEGAL_CHARACTER_STATE.process(reader, readerState);
		} else {
			final int codePoint = readResult.getResult();
			readerState.setPreviousReadCharacter(codePoint);

			readerState.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE.process(reader, readerState);
		}
	}
}
