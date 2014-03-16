package jcl.readtables.reader;

import jcl.readtables.ReadtableStruct;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;

/**
 * Step 7 of the Reader Algorithm.
 * <p/>
 * The character that was read in the ReadState is now added to the token accumulator vector and the state attributes
 * value of that character is then appended to the attributeVector.
 */
public class ConstituentState extends State {

	public static final State CONSTITUENT_STATE = new ConstituentState();

	@Override
	public void process(final StateReader reader, final ReaderState readerState) {
		Integer codePoint = readerState.getPreviousReadCharacter();

		if (StateUtils.isEndOfFileCharacter(codePoint)) {
			readerState.setReturnToken(null);

			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, readerState);
			return;
		}

		final ReadtableStruct readtable = reader.getReadtable();
		final CaseSpec readtableCase = readtable.getReadtableCase();
		final AttributeType attributeType = readtable.getAttributeType(codePoint);

		codePoint = StateUtils.properCaseCodePoint(codePoint, attributeType, readtableCase);
		readerState.addToTokenAttributes(codePoint, attributeType);

		EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE.process(reader, readerState);
	}
}
