package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.StateReader;
import jcl.reader.state.impl.util.StateUtils;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.reader.ReaderUtils;
import jcl.structs.ReadtableStruct;

/**
 * Step 7 of the Reader Algorithm.
 * <p/>
 * The character that was read in the ReadState is now added to the token accumulator vector and the state attributes
 * value of that character is then appended to the attributeVector.
 */
public class ConstituentState implements State {

	public static final State CONSTITUENT_STATE = new ConstituentState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return EvenMultiEscapeState  We have found 0 Multiple Escape Characters
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		Integer codePoint = readerState.getPreviousReadCharacter();

		if (ReaderUtils.isEndOfFileCharacter(codePoint)) {
			readerState.setReturnToken(null);
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}

		final ReadtableStruct readtable = reader.getReadtable();
		final CaseSpec readtableCase = readtable.getReadtableCase();
		final AttributeType attributeType = readtable.getAttributeType(codePoint);

		codePoint = StateUtils.properCaseCodePoint(codePoint, attributeType, readtableCase);
		readerState.addToTokenAttributes(codePoint, attributeType);

		readerState.setNextState(EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE);
		return readerState;
	}
}
