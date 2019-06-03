/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import lombok.Getter;

/**
 * Result object from a 'read' operation performed via 'read-line' from an {@link InputStreamStruct}.
 */
@Getter
public class ReadLineResult {

	/**
	 * The {@link String} result of the read operation.
	 */
	private final String result;

	/**
	 * The value to return if an End-of-File was hit during the read operation and an error was not to be thrown.
	 */
	private final LispStruct eofValue;

	/**
	 * Constructor to create a ReadLineResult with the provided {@link String} result of the read operation.
	 *
	 * @param result
	 * 		the {@link String} result of the read operation.
	 */
	public ReadLineResult(final String result) {
		this.result = result;
		eofValue = null;
	}

	/**
	 * Constructor to create a ReadLineResult with the provided {@link LispStruct} as the value to return if an
	 * End-of-File was hit during the read operation and an error was not to be thrown.
	 *
	 * @param eofValue
	 * 		the value to return if an End-of-File was hit during the read operation and an error was not to be thrown
	 */
	public ReadLineResult(final LispStruct eofValue) {
		result = null;
		this.eofValue = eofValue;
	}

	/**
	 * Returns true if either an End-of-File was hit (aka. the ReadLineResult eofValue constructor was called) or if
	 * the result object was null. The result object will always be null if the ReadLineResult eofValue constructor is
	 * called but can also be null if the value passed to the ReadLineResult result constructor is null.
	 *
	 * @return true if the ReadLineResult was an End-of-File read; false otherwise
	 */
	public boolean isEof() {
		return result == null;
	}
}
