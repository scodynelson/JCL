package jcl.structs.streams;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * Result object from a 'peek' operation performed via 'peek-char' from an {@link InputStream}.
 */
public class PeekResult {

	private final Integer result;
	private final LispStruct eofValue;
	private final boolean wasEOF;

	/**
	 * Package private constructor to create a PeekResult with the provided {@link Integer} result of the peek
	 * operation.
	 *
	 * @param result
	 * 		the {@link Integer} result of the peek operation.
	 */
	PeekResult(final Integer result) {
		this.result = result;
		eofValue = null;
		wasEOF = false;
	}

	/**
	 * Package private constructor to create a PeekResult with the provided {@link LispStruct} as the value to return
	 * if an EOF was hit during the peek operation and an error was not to be thrown.
	 *
	 * @param eofValue
	 * 		the value to return if an EOF was hit during the peek operation and an error was not to be thrown
	 */
	PeekResult(final LispStruct eofValue) {
		result = null;
		this.eofValue = eofValue;
		wasEOF = true;
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
	 * Returns true if either an EOF was hit (aka. the PeekResult eofValue constructor was called) or if the result
	 * object was null. The result object will always be null if the PeekResult eofValue constructor is called but can
	 * also be null if the value passed to the PeekResult result constructor is null.
	 *
	 * @return true if the PeekResult was an EOF peek; false otherwise
	 */
	public boolean wasEOF() {
		return wasEOF || (result == null);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}
}
