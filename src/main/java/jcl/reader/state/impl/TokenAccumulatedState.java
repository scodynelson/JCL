package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.state.StateReader;
import jcl.reader.state.TokenAttribute;
import jcl.reader.state.impl.util.StateUtils;
import jcl.reader.util.ReaderUtils;
import jcl.structs.symbols.Variable;
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
public class TokenAccumulatedState implements State {

	public static final State TOKEN_ACCUMULATED_STATE = new TokenAccumulatedState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return EndState       the final accepting state
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		final Integer codePoint = readerState.getPreviousReadCharacter();
		final LinkedList<TokenAttribute> tokenAttributes = readerState.getTokenAttributes();
		if (ReaderUtils.isEndOfFileCharacter(codePoint) && CollectionUtils.isEmpty(tokenAttributes)) {
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}

		if (Variable.ReadSuppress) {
			readerState.setReturnToken(null);
			readerState.setNextState(EndState.END_STATE);
			return readerState;
		}

		final String tokenString = StateUtils.convertTokensToString(tokenAttributes);
		if (StringUtils.equals(tokenString, ".")) {
			readerState.setErrorMessage("Dot context error in '.'");
			readerState.setNextState(ErrorState.ERROR_STATE);
		} else {
			readerState.setNextState(NumberTokenAccumulatedState.NUMBER_TOKEN_ACCUMULATED_STATE);
		}
		return readerState;
	}
}
