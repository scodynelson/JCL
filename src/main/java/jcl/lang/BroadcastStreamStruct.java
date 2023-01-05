package jcl.lang;

import java.util.ArrayDeque;
import java.util.List;

import jcl.lang.internal.stream.BroadcastStreamStructImpl;

/**
 * The {@link BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public interface BroadcastStreamStruct extends OutputStreamStruct {

	/**
	 * Returns the {@link ListStruct} of {@link OutputStreamStruct} objects that output will be broadcast to by this
	 * stream.
	 *
	 * @return the {@link ListStruct} of {@link OutputStreamStruct} objects that output will be broadcast to by this
	 * stream
	 */
	ListStruct broadcastStreamStreams();

	/**
	 * Returns a new Broadcast-Stream instance that will broadcast output to the provided {@link List} of
	 * {@link OutputStreamStruct} objects.
	 *
	 * @param outputStreamStructs
	 * 		the {@link List} of {@link OutputStreamStruct} objects to broadcast output to
	 *
	 * @return a new Broadcast-Stream instance
	 */
	static BroadcastStreamStruct toBroadcastStream(final List<OutputStreamStruct> outputStreamStructs) {
		return new BroadcastStreamStructImpl(new ArrayDeque<>(outputStreamStructs));
	}
}
