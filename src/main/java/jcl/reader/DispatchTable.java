/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * This holds mappings for code points to macro functions and delegates to the proper {@link ReaderMacroFunction} when
 * used.
 */
public class DispatchTable extends ReaderMacroFunction {

	/**
	 * The internal mapping of character code points to {@link ReaderMacroFunction}s to dispatch on when reading.
	 */
	private final Map<Integer, ReaderMacroFunction> macroFunctionMap = new ConcurrentHashMap<>();

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {

		final ReadPeekResult readResult = reader.readChar(false, null, false);
		if (readResult.isEof()) {
			throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
		}

		final int readChar = readResult.getResult();
		final ReaderMacroFunction macroFunction = getMacroFunction(readChar);
		if (macroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for: " + codePoint + readChar + '.');
		}

		return macroFunction.readMacro(readChar, reader, numArg);
	}

	/**
	 * Gets the macro function associated with the provided {@code codePoint}, or null if no such function exists.
	 *
	 * @param codePoint
	 * 		the code point associated with the macro function to retrieve
	 *
	 * @return the macro function associated with the provided {@code codePoint}, or null if no such function exists
	 */
	public ReaderMacroFunction getMacroFunction(final int codePoint) {
		return macroFunctionMap.get(codePoint);
	}

	/**
	 * Sets the macro function with the provided {@code codePoint} to the provided {@link ReaderMacroFunction}.
	 *
	 * @param codePoint
	 * 		the code point associated with the macro function to set
	 * @param macroFunction
	 * 		the new macro function to be associated
	 */
	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction macroFunction) {
		macroFunctionMap.put(codePoint, macroFunction);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
