package jcl.readtables.reader;

import jcl.syntax.reader.TokenAttribute;
import jcl.variables.ReadSuppressVariable;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;

/**
 * Step 10 of the Reader Algorithm.
 * <p/>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Number/PotentialNumber
 * 2) Symbol
 * 3) Package with a Symbol
 * <p/>
 * First we check to see if the token is a number, if it is, then we attempt to format it.  If it cannot
 * be formatted, then we intern it as a Symbol.
 * <p/>
 * If it is not a number, then we attempt to see if it contains any Package Markers, if it does, then
 * we attempt to get the package for it based on 3 formats.  The formats are as follows:
 * <p/>
 * 1) ":SYMBOL_NAME" - This format should find the symbol in the Keyword Package.
 * <p/>
 * 2) "PACKAGE_NAME:SYMBOL_NAME" - This format will have to find the package and then find the
 * symbol that is external to that package.
 * <p/>
 * 3) "PACKAGE_NAME::SYMBOL_NAME" - This format will have to find the package and then intern the
 * symbol that is internal to that package.
 * <p/>
 * 4) Any other combinations of Package Markers will result in an error.
 * <p/>
 * After the token has been made into an object, that object is set as the return object for the read
 * function.  We then return the EndState.
 * <p/>
 * This will have to be fixed because it only handles Symbols using the Common Lisp Package
 * and Symbols that are in the Keyword Package!!!
 * <p/>
 */
public class TokenAccumulatedState extends State {

	public static final State TOKEN_ACCUMULATED_STATE = new TokenAccumulatedState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return EndState       the final accepting state
	 */
	@Override
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {

		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();
		final LinkedList<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();
		if (StateUtils.isEndOfFileCharacter(codePoint) && CollectionUtils.isEmpty(tokenAttributes)) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			tokenBuilder.setReturnToken(null);
			return;
		}

		final String tokenString = StateUtils.convertTokensToString(tokenAttributes);
		if (StringUtils.equals(tokenString, ".")) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.setErrorMessage("Dot context error in '.'");
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
		} else {
			NumberTokenAccumulatedState.NUMBER_TOKEN_ACCUMULATED_STATE.process(reader, tokenBuilder);
		}
	}
}
