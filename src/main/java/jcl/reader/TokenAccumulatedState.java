package jcl.reader;

import jcl.reader.syntax.TokenAttribute;
import jcl.reader.syntax.TokenBuilder;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.variables.Variable;

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
public class TokenAccumulatedState extends State {

	public static final State TOKEN_ACCUMULATED_STATE = new TokenAccumulatedState();

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();
		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();
		if (isEndOfFileCharacter(codePoint) && tokenAttributes.isEmpty()) {
			final ErrorState errorState = new ErrorState(this);
			errorState.process(reader, tokenBuilder);
			return;
		}

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			tokenBuilder.setReturnToken(null);
			return;
		}

		final String tokenString = convertTokensToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		} else {
			NumberTokenAccumulatedState.NUMBER_TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		}
	}
}
