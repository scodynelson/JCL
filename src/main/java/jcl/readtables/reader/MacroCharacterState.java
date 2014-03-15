package jcl.readtables.reader;

import jcl.readtables.macrofunction.ReaderMacroFunction;
import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.syntax.reader.ReadResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Step 4 of the Reader Algorithm.
 * <p/>
 * The way a ReaderMacroFunction is called takes the following 3 steps:
 * <p/>
 * First: The Readtable is called to get the macro function of a specific character:
 * this will return an instance of ReaderMacroFunction
 * Second: We then call the readMacro method from the ReaderMacroFunction instance that is returned from step 1
 * Third: The readMacro will then return a LispStruct.  If it is not null, we return
 * it as a result of the Read function, else, Step 1 is re-entered
 * <p/>
 */
public class MacroCharacterState implements State {

	public static final State MACRO_CHARACTER_STATE = new MacroCharacterState();

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroCharacterState.class);

	/**
	 * Processes for the reader for the current State.
	 *
	 * @return ReadState     if the return value of the ReaderMacroFunction is null
	 * EndState      if the return value of the ReaderMacroFunction is not null
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		readerState.setPreviousState(this);

		Integer codePoint = readerState.getPreviousReadCharacter();

		if (ReaderUtils.isEndOfFileCharacter(codePoint)) {
			readerState.setReturnToken(null);
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}

		Integer numArg = null;
		if (Character.isDigit(codePoint)) {

			final StringBuilder digitStringBuilder = new StringBuilder();
			digitStringBuilder.append(codePoint);

			// NOTE: This will throw errors when it reaches an EOF
			ReadResult readResult = reader.readChar();
			int readChar = readResult.getResult();
			while (Character.isDigit(readChar)) {
				digitStringBuilder.appendCodePoint(readChar);

				readResult = reader.readChar();
				readChar = readResult.getResult();
			}

			final String digitString = digitStringBuilder.toString();
			try {
				numArg = Integer.parseInt(digitString);
			} catch (final NumberFormatException nfe) {
				final String errorString = '"' + digitString + "\" does not represent an integer.";
				LOGGER.error(errorString, nfe);
				readerState.setErrorMessage(errorString);
				readerState.setNextState(ErrorState.ERROR_STATE);
				return readerState;
			}

			codePoint = readChar;
		}

		final ReaderMacroFunction readerMacroFunction = reader.getReadtable().getMacroCharacter(codePoint);
		if (readerMacroFunction == null) {
			readerState.setErrorMessage("No reader macro function exists for character: " + codePoint + '.');
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}

		final LispStruct lispToken;
		try {
			final MacroFunctionReader macroFunctionReader = new MacroFunctionReader(reader);
			lispToken = readerMacroFunction.readMacro(codePoint, macroFunctionReader, numArg);
		} catch (final ReaderErrorException re) {
			final String errorString = re.getMessage();
			LOGGER.error(errorString, re);
			readerState.setErrorMessage(errorString);
			readerState.setNextState(ErrorState.ERROR_STATE);
			return readerState;
		}
		readerState.setReturnToken(lispToken);

		final State nextState;
		if (lispToken == null) {
			nextState = ReadState.READ_STATE;
		} else {
			nextState = EndState.END_STATE;
		}

		readerState.setNextState(nextState);
		return readerState;
	}
}
