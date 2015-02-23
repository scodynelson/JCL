/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
@Component
class MacroCharacterReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6724298209965623929L;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public SimpleElement process(final TokenBuilder tokenBuilder) {

		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();
		final int codePoint = readResult.getResult(); // This will not be 'null'. We check for EOFs after each 'read'.

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		final ReaderMacroFunction readerMacroFunction = readtable.getMacroCharacter(codePoint);

		if (readerMacroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for character: " + codePoint + '.');
		}

		final Reader reader = tokenBuilder.getReader();

		BigInteger numArg = null;
		if (readerMacroFunction.isDispatch()) {
			numArg = getNumberArgument(reader);
		}

		final SimpleElement element = readerMacroFunction.readMacro(codePoint, reader, numArg);
		if (element == null) {
			return readerStateMediator.read(tokenBuilder);
		} else {
			return element;
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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
