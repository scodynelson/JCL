package jcl.reader;

import jcl.LispStruct;
import jcl.structs.streams.ReadResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 5 of the Reader Algorithm.
 * <p>
 * If x is a single escape character then the next character, y, is read, or an error of type end-of-file is signaled
 * if at the end of file. y is treated as if it is a constituent whose only constituent trait is alphabetic[2]. y is
 * used to begin a token, and step 8 is entered.
 * </p>
 */
@Component
class SingleEscapeState extends State {

	@Autowired
	private IllegalCharacterState illegalCharacterState;

	@Autowired
	private EvenMultiEscapeState evenMultiEscapeState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			illegalCharacterState.process(reader, tokenBuilder);
		} else {
			final int codePoint = readResult.getResult();
			tokenBuilder.setPreviousReadCharacter(codePoint);

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			evenMultiEscapeState.process(reader, tokenBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
