package jcl.structs.streams;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Result object from a 'read' operation performed via 'read-char' or 'read-byte' from an {@link InputStream}.
 */
public class ReadResult {

	private final Integer result;
	private final LispStruct eofValue;
	private final boolean wasEOF;

	/**
	 * Package private constructor to create a ReadResult with the provided {@link Integer} result of the read
	 * operation.
	 *
	 * @param result
	 * 		the {@link Integer} result of the read operation.
	 */
	ReadResult(final Integer result) {
		this.result = result;
		eofValue = null;
		wasEOF = false;
	}

	/**
	 * Package private constructor to create a ReadResult with the provided {@link LispStruct} as the value to return
	 * if an EOF was hit during the read operation and an error was not to be thrown.
	 *
	 * @param eofValue
	 * 		the value to return if an EOF was hit during the read operation and an error was not to be thrown
	 */
	ReadResult(final LispStruct eofValue) {
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
	 * Returns true if either an EOF was hit (aka. the ReadResult eofValue constructor was called) or if the result
	 * object was null. The result object will always be null if the ReadResult eofValue constructor is called but can
	 * also be null if the value passed to the ReadResult result constructor is null.
	 *
	 * @return true if the ReadResult was an EOF read; false otherwise
	 */
	public boolean wasEOF() {
		return wasEOF || (result == null);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
