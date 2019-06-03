package jcl.lang;

import jcl.lang.internal.stream.EchoStreamStructImpl;

/**
 * The {@link EchoStreamStruct} is the object representation of a Lisp 'echo-stream' type.
 */
public interface EchoStreamStruct extends IOStreamStruct {

	/**
	 * Returns the {@link InputStreamStruct} where input will be read from.
	 *
	 * @return the {@link InputStreamStruct} where input will be read from
	 */
	InputStreamStruct echoStreamInputStream();

	/**
	 * Returns the {@link OutputStreamStruct} where output will be written to.
	 *
	 * @return the {@link OutputStreamStruct} where output will be written to
	 */
	OutputStreamStruct echoStreamOutputStream();

	/**
	 * Returns a new Echo-Stream instance that will read input from the provided {@link InputStreamStruct} and write
	 * output to the provided {@link OutputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} where input will be read from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} where output will be written to
	 *
	 * @return a new Echo-Stream instance
	 */
	static EchoStreamStruct toEchoStream(final InputStreamStruct inputStreamStruct,
	                                     final OutputStreamStruct outputStreamStruct) {
		return new EchoStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}
}
