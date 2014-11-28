/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.ReaderMacroFunction;
import jcl.streams.ReadPeekResult;

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
final class MacroCharacterState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new MacroCharacterState();

	/**
	 * Private constructor.
	 */
	private MacroCharacterState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (State.isEndOfFileCharacter(codePoint)) {
			State.handleEndOfFile(tokenBuilder, "MacroCharacterState");
			return;
		}

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final ReaderMacroFunction readerMacroFunction = readtable.getMacroCharacter(codePoint);
		if (readerMacroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for character: " + codePoint + '.');
		}

		BigInteger numArg = null;
		if (readerMacroFunction.isDispatch()) {
			numArg = getNumberArgument(reader);
		}

		final LispStruct lispToken = readerMacroFunction.readMacro(codePoint, reader, numArg);
		tokenBuilder.setReturnToken(lispToken);

		if (lispToken == null) {
			readerStateMediator.read(reader, tokenBuilder);
		}
	}

	/**
	 * Reads in the number argument for the macro reader to use when processing the macro function character.
	 *
	 * @param reader
	 * 		the reader to use for reading the number argument
	 *
	 * @return the number argument if it exists; null if no number argument was read in
	 */
	private static BigInteger getNumberArgument(final Reader reader) {

		// NOTE: This will throw errors when it reaches an EOF. That's why we can un-box the 'readChar' variable below.
		ReadPeekResult readResult = reader.readChar();
		int readChar = readResult.getResult();

		final StringBuilder digitStringBuilder = new StringBuilder();

		while (Character.isDigit(readChar)) {
			digitStringBuilder.appendCodePoint(readChar);

			readResult = reader.readChar();
			readChar = readResult.getResult();
		}

		final int minimumDigitLength = 1;

		BigInteger numArg = null;
		if (digitStringBuilder.length() >= minimumDigitLength) {
			final String digitString = digitStringBuilder.toString();
			numArg = new BigInteger(digitString);
		}

		// Make sure to unread the last character read after the number arg
		reader.unreadChar(readChar);

		return numArg;
	}
}
