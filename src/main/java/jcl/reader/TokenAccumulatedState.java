package jcl.reader;

import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;

/**
 * Step 10 of the Reader Algorithm.
 * <p>
 * An entire token has been accumulated. The object represented by the token is returned as the result of the read
 * operation, or an error of type reader-error is signaled if the token is not of valid syntax.
 * </p>
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Number/PotentialNumber
 * 2) Symbol
 * 3) Package with a Symbol
 * </p>
 */
@Component
class TokenAccumulatedState extends State {

	@Autowired
	private NumberTokenAccumulatedState numberTokenAccumulatedState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();
		if (isEndOfFileCharacter(codePoint) && tokenAttributes.isEmpty()) {
			if (tokenBuilder.isEofErrorP()) {
				throw new ReaderErrorException("End-of-File encountered in State: " + this);
			} else {
				tokenBuilder.setReturnToken(null);
				return;
			}
		}

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			tokenBuilder.setReturnToken(null);
			return;
		}

		final String tokenString = convertTokensToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		} else {
			numberTokenAccumulatedState.process(reader, tokenBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
