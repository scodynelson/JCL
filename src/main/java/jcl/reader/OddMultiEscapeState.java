package jcl.reader;

import jcl.LispStruct;
import jcl.structs.streams.ReadResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 9 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an odd number of multiple escape characters have been encountered.
 * If at end of file, an error of type end-of-file is signaled. Otherwise, a character, y, is read, and one of the
 * following actions is performed according to its syntax type:
 * <tab>
 * <p>
 * If y is a constituent, macro, or whitespace character, y is treated as a constituent whose only constituent trait is
 * alphabetic. Y is appended to the token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
 * if at end of file. Z is treated as a constituent whose only constituent trait is alphabetic. Z is appended to the
 * token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a multiple escape character, then step 8 is entered.
 * </p>
 * <p>
 * If y is an invalid character, an error of type reader-error is signaled.
 * </p>
 * </tab>
 * </p>
 */
@Component
class OddMultiEscapeState extends State {

	@Autowired
	private IllegalCharacterState illegalCharacterState;

	@Autowired
	private EvenMultiEscapeState evenMultiEscapeState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			illegalCharacterState.process(reader, tokenBuilder);
		}

		int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final SyntaxType syntaxType = reader.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
			if (readResult.wasEOF()) {
				illegalCharacterState.process(reader, tokenBuilder);
			} else {
				codePoint = readResult.getResult();
				tokenBuilder.setPreviousReadCharacter(codePoint);
				tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

				process(reader, tokenBuilder);
			}
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			evenMultiEscapeState.process(reader, tokenBuilder);
		} else {
			illegalCharacterState.process(reader, tokenBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
