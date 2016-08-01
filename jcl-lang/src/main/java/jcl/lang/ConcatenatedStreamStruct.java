package jcl.lang;

import java.util.Deque;

/**
 * The {@link ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public interface ConcatenatedStreamStruct extends InputStreamStruct {

	Deque<InputStreamStruct> getInputStreamStructs();
}
