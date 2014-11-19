package jcl.reader;

import jcl.LispStruct;
import jcl.reader.function.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.TokenBuilder;
import jcl.structs.streams.ReadResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

/**
 * Step 4 of the Reader Algorithm.
 * <p>
 * If x is a terminating or non-terminating macro character then its associated reader macro function is called with
 * two arguments, the input stream and x.
 * <tab>
 * <p>
 * The reader macro function may read characters from the input stream; if it does, it will see those characters
 * following the macro character. The Lisp reader may be invoked recursively from the reader macro function.
 * </p>
 * <p>
 * The reader macro function must not have any side effects other than on the input stream; because of backtracking and
 * restarting of the read operation, front ends to the Lisp reader (e.g., ``editors'' and ``rubout handlers'') may
 * cause the reader macro function to be called repeatedly during the reading of a single expression in which x only
 * appears once.
 * </p>
 * <p>
 * The reader macro function may return zero values or one value. If one value is returned, then that value is returned
 * as the result of the read operation; the algorithm is done. If zero values are returned, then step 1 is re-entered.
 * </p>
 * </tab>
 * </p>
 */
public class MacroCharacterState extends State {

	public static final State MACRO_CHARACTER_STATE = new MacroCharacterState();

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroCharacterState.class);

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (isEndOfFileCharacter(codePoint)) {
			tokenBuilder.setReturnToken(null);

			final ErrorState errorState = new ErrorState(this, null);
			errorState.process(reader, tokenBuilder);
			return;
		}

		final ReaderMacroFunction readerMacroFunction = reader.getReadtable().getMacroCharacter(codePoint);
		if (readerMacroFunction == null) {
			final String errorMessage = "No reader macro function exists for character: " + codePoint + '.';
			final ErrorState errorState = new ErrorState(this, errorMessage);
			errorState.process(reader, tokenBuilder);
			return;
		}

		BigInteger numArg = null;
		if (readerMacroFunction instanceof DispatchTable) {
			numArg = getNumberArgument(reader);
		}

//		try {
		final LispStruct lispToken = readerMacroFunction.readMacro(codePoint, reader, numArg);
		tokenBuilder.setReturnToken(lispToken);

		if (lispToken == null) {
			ReadState.READ_STATE.process(reader, tokenBuilder);
		}
//		} catch (final ReaderErrorException re) {
//			final String errorString = re.getMessage();
//			LOGGER.error(errorString, re);
//
//			ErrorState.ERROR_STATE.setPreviousState(this);
//			ErrorState.ERROR_STATE.setErrorMessage(errorString);
//			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
//		}
	}

	private static BigInteger getNumberArgument(final Reader reader) {

		// NOTE: This will throw errors when it reaches an EOF
		ReadResult readResult = reader.readChar();
		int readChar = readResult.getResult();

		final StringBuilder digitStringBuilder = new StringBuilder();

		while (Character.isDigit(readChar)) {
			digitStringBuilder.appendCodePoint(readChar);

			readResult = reader.readChar();
			readChar = readResult.getResult();
		}

		BigInteger numArg = null;
		if (digitStringBuilder.length() != 0) {
			final String digitString = digitStringBuilder.toString();
			numArg = new BigInteger(digitString);

		}

		// Make sure to unread the last character read after the number arg
		reader.unreadChar(readChar);

		return numArg;
	}
}
