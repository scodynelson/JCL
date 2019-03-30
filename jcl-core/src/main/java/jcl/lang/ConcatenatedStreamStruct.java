package jcl.lang;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import jcl.lang.internal.stream.ConcatenatedStreamStructImpl;

/**
 * The {@link ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public interface ConcatenatedStreamStruct extends InputStreamStruct {

	Deque<InputStreamStruct> getInputStreamStructs();

	default ListStruct concatenatedStreamStreams() {
		final Deque<InputStreamStruct> inputStreamStructs = getInputStreamStructs();
		return ListStruct.toLispList(new ArrayList<LispStruct>(inputStreamStructs));
	}

	static ConcatenatedStreamStruct toConcatenatedStream(final List<InputStreamStruct> inputStreamStructs) {
		return new ConcatenatedStreamStructImpl(new ArrayDeque<>(inputStreamStructs));
	}

	static ConcatenatedStreamStruct toConcatenatedStream(final Deque<InputStreamStruct> inputStreamStructs) {
		return new ConcatenatedStreamStructImpl(inputStreamStructs);
	}
}
