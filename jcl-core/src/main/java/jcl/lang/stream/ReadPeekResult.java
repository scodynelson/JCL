/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;

/**
 * Result object from a 'read' or 'peek' operation performed via 'read-char', 'peek-char', or 'read-byte' from an
 * {@link InputStreamStruct}.
 */
public class ReadPeekResult {

	/**
	 * The {@link Integer} result of the read or peek operation.
	 */
	private final Integer result;

	/**
	 * The value to return if an End-of-File was hit during the read or peek operation and an error was not to be
	 * thrown.
	 */
	private final LispStruct eofValue;

	/**
	 * Whether or not an End-of-File occurred.
	 */
	private final boolean eof;

	/**
	 * Package private constructor to create a ReadPeekResult with the provided {@link Integer} result of the read or
	 * peek operation.
	 *
	 * @param result
	 * 		the {@link Integer} result of the read or peek operation.
	 */
	public ReadPeekResult(final Integer result) {
		this.result = result;
		eofValue = null;
		eof = false;
	}

	/**
	 * Package private constructor to create a ReadPeekResult with the provided {@link LispStruct} as the value to
	 * return if an End-of-File was hit during the read or peek operation and an error was not to be thrown.
	 *
	 * @param eofValue
	 * 		the value to return if an End-of-File was hit during the read or peek operation and an error was not to be
	 * 		thrown
	 */
	public ReadPeekResult(final LispStruct eofValue) {
		result = null;
		this.eofValue = eofValue;
		eof = true;
	}

	/**
	 * Getter for the {@link #result} value.
	 *
	 * @return the {@link #result} value
	 */
	public Integer getResult() {
		return result;
	}

	/**
	 * Getter for the {@link #eofValue} value.
	 *
	 * @return the {@link #eofValue} value
	 */
	public LispStruct getEofValue() {
		return eofValue;
	}

	/**
	 * Returns true if either an End-of-File was hit (aka. the ReadPeekResult eofValue constructor was called) or if
	 * the result object was null. The result object will always be null if the ReadPeekResult eofValue constructor is
	 * called but can also be null if the value passed to the ReadPeekResult result constructor is null.
	 *
	 * @return true if the ReadPeekResult was an End-of-File read; false otherwise
	 */
	public boolean isEof() {
		return eof || (result == null);
	}
}