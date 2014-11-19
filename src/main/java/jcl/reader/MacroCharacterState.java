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
 * The way a ReaderMacroFunction is called takes the following 3 steps:
 * <p>
 * First: The Readtable is called to get the macro function of a specific character:
 * this will return an instance of ReaderMacroFunction
 * Second: We then call the readMacro method from the ReaderMacroFunction instance that is returned from step 1
 * Third: The readMacro will then return a LispStruct.  If it is not null, we return
 * it as a result of the Read function, else, Step 1 is re-entered
 * <p>
 */
public class MacroCharacterState extends State {

	public static final State MACRO_CHARACTER_STATE = new MacroCharacterState();

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroCharacterState.class);

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {

		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (isEndOfFileCharacter(codePoint)) {
			tokenBuilder.setReturnToken(null);

			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

		final ReaderMacroFunction readerMacroFunction = reader.getReadtable().getMacroCharacter(codePoint);
		if (readerMacroFunction == null) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.setErrorMessage("No reader macro function exists for character: " + codePoint + '.');
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
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
