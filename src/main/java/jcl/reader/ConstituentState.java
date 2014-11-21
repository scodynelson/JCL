package jcl.reader;

import jcl.structs.conditions.exceptions.ReaderErrorException;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 7 of the Reader Algorithm.
 * <p>
 * If x is a constituent character, then it begins a token. After the token is read in, it will be interpreted either
 * as a Lisp object or as being of invalid syntax. If the token represents an object, that object is returned as the
 * result of the read operation. If the token is of invalid syntax, an error is signaled. If x is a character with
 * case, it might be replaced with the corresponding character of the opposite case, depending on the readtable case of
 * the current readtable. X is used to begin a token, and step 8 is entered.
 * </p>
 */
@Component
class ConstituentState extends State {

	@Autowired
	private EvenMultiEscapeState evenMultiEscapeState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (isEndOfFileCharacter(codePoint)) {
			if (tokenBuilder.isEofErrorP()) {
				throw new ReaderErrorException("End-of-File encountered in State: " + this);
			} else {
				tokenBuilder.setReturnToken(null);
				return;
			}
		}

		final CaseSpec readtableCase = reader.getReadtableCase();
		final AttributeType attributeType = reader.getAttributeType(codePoint);

		codePoint = properCaseCodePoint(codePoint, attributeType, readtableCase);
		tokenBuilder.addToTokenAttributes(codePoint, attributeType);

		evenMultiEscapeState.process(reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
