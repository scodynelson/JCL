/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.Getter;

/**
 * Result object from a 'read' operation performed via 'read-char' or 'read-byte' from an {@link InputStreamStruct}.
 */
@Getter
public final class ReadCharResult {

	/**
	 * The {@link Integer} result of the read operation.
	 */
	private final Integer result;

	/**
	 * The value to return if an End-of-File was hit during the read operation and an error was not to be thrown.
	 */
	private final LispStruct eofValue;

	/**
	 * Constructor to create a ReadCharResult with the provided {@link Integer} result of the read  operation.
	 *
	 * @param result
	 * 		the {@link Integer} result of the read operation.
	 */
	public ReadCharResult(final Integer result) {
		this.result = result;
		eofValue = null;
	}

	/**
	 * Constructor to create a ReadCharResult with the provided {@link LispStruct} as the value to return if an
	 * End-of-File was hit during the read operation and an error was not to be thrown.
	 *
	 * @param eofValue
	 * 		the value to return if an End-of-File was hit during the read operation and an error was not to be thrown
	 */
	public ReadCharResult(final LispStruct eofValue) {
		result = null;
		this.eofValue = eofValue;
	}

	/**
	 * Returns true if either an End-of-File was hit (aka. the ReadCharResult eofValue constructor was called) or if the
	 * result object was null. The result object will always be null if the ReadCharResult eofValue constructor is
	 * called but can also be null if the value passed to the ReadCharResult result constructor is null.
	 *
	 * @return true if the ReadCharResult was an End-of-File read; false otherwise
	 */
	public boolean isEof() {
		return result == null;
	}
}
