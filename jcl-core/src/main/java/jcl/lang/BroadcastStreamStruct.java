package jcl.lang;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import jcl.lang.internal.stream.BroadcastStreamStructImpl;

/**
 * The {@link BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public interface BroadcastStreamStruct extends OutputStreamStruct {

	Deque<OutputStreamStruct> getOutputStreamStructs();

	default ListStruct broadcastStreamStreams() {
		final Deque<OutputStreamStruct> outputStreamStructs = getOutputStreamStructs();
		return ListStruct.toLispList(new ArrayList<LispStruct>(outputStreamStructs));
	}

	static BroadcastStreamStruct toBroadcastStream(final List<OutputStreamStruct> outputStreamStructs) {
		return new BroadcastStreamStructImpl(new ArrayDeque<>(outputStreamStructs));
	}

	static BroadcastStreamStruct toBroadcastStream(final Deque<OutputStreamStruct> outputStreamStructs) {
		return new BroadcastStreamStructImpl(outputStreamStructs);
	}
}
