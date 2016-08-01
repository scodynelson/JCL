package jcl.lang;

import java.util.Deque;

/**
 * The {@link BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public interface BroadcastStreamStruct extends OutputStreamStruct {

	Deque<OutputStreamStruct> getOutputStreamStructs();
}
