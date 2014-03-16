package jcl.readtables.reader.impl.states.impl;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.MacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.readtables.reader.impl.macrofunctions.impl.MacroFunctionReaderImpl;
import jcl.readtables.reader.impl.states.TokenBuilder;
import jcl.readtables.reader.macrofunction.ReaderMacroFunction;
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
public class MacroCharacterState extends State {

	public static final State MACRO_CHARACTER_STATE = new MacroCharacterState();

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroCharacterState.class);

	@Override
	public void process(final StateReader reader, final TokenBuilder tokenBuilder) {

		Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (StateUtils.isEndOfFileCharacter(codePoint)) {
			tokenBuilder.setReturnToken(null);

			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

		final Integer numArg;
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
//			try {
			numArg = Integer.parseInt(digitString);
//			} catch (final NumberFormatException nfe) {
//				final String errorString = '"' + digitString + "\" does not represent an integer.";
//				LOGGER.error(errorString, nfe);
//
//				ErrorState.ERROR_STATE.setPreviousState(this);
//				ErrorState.ERROR_STATE.setErrorMessage(errorString);
//				ErrorState.ERROR_STATE.process(reader, tokenBuilder);
//				return;
//			}

			codePoint = readChar;
		} else {
			numArg = null;
		}

		final ReaderMacroFunction readerMacroFunction = reader.getReadtable().getMacroCharacter(codePoint);
		if (readerMacroFunction == null) {
			ErrorState.ERROR_STATE.setPreviousState(this);
			ErrorState.ERROR_STATE.setErrorMessage("No reader macro function exists for character: " + codePoint + '.');
			ErrorState.ERROR_STATE.process(reader, tokenBuilder);
			return;
		}

//		try {
		final MacroFunctionReader macroFunctionReader = new MacroFunctionReaderImpl(reader);
		final LispStruct lispToken = readerMacroFunction.readMacro(codePoint, macroFunctionReader, numArg);
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
}
