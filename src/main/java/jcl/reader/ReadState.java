package jcl.reader;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.ReadResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 1 of the Reader Algorithm.
 * <p>
 * If at end of file, end-of-file processing is performed as specified in read. Otherwise, one character, x, is read
 * from the input stream, and dispatched according to the syntax type of x to one of steps 2 to 7.
 * </p>
 */
@Component
class ReadState extends State {

	@Autowired
	private IllegalCharacterState illegalCharacterState;

	@Autowired
	private WhitespaceState whitespaceState;

	@Autowired
	private MacroCharacterState macroCharacterState;

	@Autowired
	private SingleEscapeState singleEscapeState;

	@Autowired
	private MultipleEscapeState multipleEscapeState;

	@Autowired
	private ConstituentState constituentState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			if (tokenBuilder.isEofErrorP()) {
				throw new ReaderErrorException("End-of-File encountered in State: " + this);
			} else {
				tokenBuilder.setReturnToken(null);
				return;
			}
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.setPreviousReadCharacter(codePoint);

		final SyntaxType syntaxType = reader.getSyntaxType(codePoint);

		if (syntaxType == SyntaxType.WHITESPACE) {
			whitespaceState.process(reader, tokenBuilder);
		} else if ((syntaxType == SyntaxType.TERMINATING) || (syntaxType == SyntaxType.NON_TERMINATING)) {
			macroCharacterState.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {
			singleEscapeState.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			multipleEscapeState.process(reader, tokenBuilder);
		} else if (syntaxType == SyntaxType.CONSTITUENT) {
			constituentState.process(reader, tokenBuilder);
		} else {
			illegalCharacterState.process(reader, tokenBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
