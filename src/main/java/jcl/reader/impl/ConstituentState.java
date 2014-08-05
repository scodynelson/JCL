package jcl.reader.impl;

import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.TokenBuilder;

/**
 * Step 7 of the Reader Algorithm.
 * <p>
 * The character that was read in the ReadState is now added to the token accumulator vector and the state attributes
 * value of that character is then appended to the attributeVector.
 */
public class ConstituentState extends State {

	public static final State CONSTITUENT_STATE = new ConstituentState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (StateUtils.isEndOfFileCharacter(codePoint)) {
			tokenBuilder.setReturnToken(null);

			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

		final CaseSpec readtableCase = reader.getReadtableCase();
		final AttributeType attributeType = reader.getAttributeType(codePoint);

		codePoint = StateUtils.properCaseCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		EvenMultiEscapeState.EVEN_MULTI_ESCAPE_STATE.process(reader, tokenBuilder);
	}
}
