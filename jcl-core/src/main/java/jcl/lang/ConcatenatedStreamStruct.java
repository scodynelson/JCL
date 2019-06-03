package jcl.lang;

import java.util.ArrayDeque;
import java.util.List;

import jcl.lang.internal.stream.ConcatenatedStreamStructImpl;

/**
 * The {@link ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public interface ConcatenatedStreamStruct extends InputStreamStruct {

	/**
	 * Returns the {@link ListStruct} of {@link InputStreamStruct} objects that input is accepted from for this stream.
	 *
	 * @return the {@link ListStruct} of {@link InputStreamStruct} objects that input is accepted from for this stream
	 */
	ListStruct concatenatedStreamStreams();

	/**
	 * Returns a new Concatenated-Stream instance that will accept input from each of the provided {@link List} of
	 * {@link InputStreamStruct} objects in order.
	 *
	 * @param inputStreamStructs
	 * 		the {@link List} of {@link InputStreamStruct} objects to accept input from
	 *
	 * @return a new Concatenated-Stream instance
	 */
	static ConcatenatedStreamStruct toConcatenatedStream(final List<InputStreamStruct> inputStreamStructs) {
		return new ConcatenatedStreamStructImpl(new ArrayDeque<>(inputStreamStructs));
	}
}
