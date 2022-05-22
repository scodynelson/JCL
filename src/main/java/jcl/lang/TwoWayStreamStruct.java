package jcl.lang;

import jcl.lang.internal.stream.TwoWayStreamStructImpl;

/**
 * The {@link TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public interface TwoWayStreamStruct extends IOStreamStruct {

	/**
	 * Returns the {@link InputStreamStruct} where input will be read from.
	 *
	 * @return the {@link InputStreamStruct} where input will be read from
	 */
	InputStreamStruct twoWayStreamInputStream();

	/**
	 * Returns the {@link OutputStreamStruct} where output will be written to.
	 *
	 * @return the {@link OutputStreamStruct} where output will be written to
	 */
	OutputStreamStruct twoWayStreamOutputStream();

	/**
	 * Returns a new Two-Way-Stream instance that will read input from the provided {@link InputStreamStruct} and write
	 * output to the provided {@link OutputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} where input will be read from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} where output will be written to
	 *
	 * @return a new Two-Way-Stream instance
	 */
	static TwoWayStreamStruct toTwoWayStream(final InputStreamStruct inputStreamStruct,
	                                         final OutputStreamStruct outputStreamStruct) {
		return new TwoWayStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}
}
