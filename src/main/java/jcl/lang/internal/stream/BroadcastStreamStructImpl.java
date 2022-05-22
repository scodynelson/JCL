/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;

import jcl.lang.BooleanStruct;
import jcl.lang.BroadcastStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link BroadcastStreamStructImpl} is the object representation of a Lisp 'broadcast-stream' type.
 */
public final class BroadcastStreamStructImpl extends StreamStructImpl implements BroadcastStreamStruct {

	/**
	 * This {@link Deque} of {@link OutputStreamStruct} objects where output is broadcast to.
	 */
	private final Deque<OutputStreamStruct> outputStreamStructs;

	/**
	 * Public constructor, initializing the provided the {@link Deque} of {@link OutputStreamStruct} objects.
	 *
	 * @param outputStreamStructs
	 * 		the {@link Deque} of {@link OutputStreamStruct} objects to initialize
	 */
	public BroadcastStreamStructImpl(final Deque<OutputStreamStruct> outputStreamStructs) {
		super(getElementType(outputStreamStructs));
		this.outputStreamStructs = new ArrayDeque<>(outputStreamStructs);
	}

	/**
	 * Retrieves the current element type for the {@link Deque} of {@link OutputStreamStruct} objects.
	 *
	 * @param outputStreamStructs
	 * 		the {@link Deque} of {@link OutputStreamStruct} objects to retrieve the element type from
	 *
	 * @return the current element type for the {@link Deque} of {@link OutputStreamStruct} objects
	 */
	private static LispStruct getElementType(final Deque<OutputStreamStruct> outputStreamStructs) {
		if (outputStreamStructs.isEmpty()) {
			return CommonLispSymbols.T;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.streamElementType();
	}

	/*
	BROADCAST-STREAM-STRUCT
	 */

	@Override
	public ListStruct broadcastStreamStreams() {
		return ListStruct.toLispList(new ArrayList<LispStruct>(outputStreamStructs));
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		outputStreamStructs.forEach(e -> e.writeChar(codePoint));
	}

	@Override
	public void writeByte(final int aByte) {
		outputStreamStructs.forEach(e -> e.writeChar(aByte));
	}

	@Override
	public void writeString(final String outputString) {
		outputStreamStructs.forEach(e -> e.writeString(outputString));
	}

	@Override
	public void writeLine(final String outputString) {
		outputStreamStructs.forEach(e -> e.writeLine(outputString));
	}

	@Override
	public BooleanStruct freshLine() {
		BooleanStruct wroteALine = NILStruct.INSTANCE;
		for (final OutputStreamStruct outputStreamStruct : outputStreamStructs) {
			final BooleanStruct result = outputStreamStruct.freshLine();
			if (result == TStruct.INSTANCE) {
				wroteALine = TStruct.INSTANCE;
			}
		}
		return wroteALine;
	}

	@Override
	public BooleanStruct terpri() {
		outputStreamStructs.forEach(OutputStreamStruct::terpri);
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct clearOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::clearOutput);
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct finishOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::finishOutput);
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct forceOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::forceOutput);
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		if (abort.toJavaPBoolean()) {
			clearOutput();
		} else {
			forceOutput();
		}
		return super.close(abort);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * The element type can differ based on the state of the {@link #outputStreamStructs} {@link Deque}.
	 *
	 * @return the current stream-element-type
	 */
	@Override
	public LispStruct streamElementType() {
		return getElementType(outputStreamStructs);
	}

	@Override
	public LispStruct fileLength() {
		if (outputStreamStructs.isEmpty()) {
			return IntegerStruct.ZERO;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.fileLength();
	}

	@Override
	public LispStruct filePosition() {
		if (outputStreamStructs.isEmpty()) {
			return IntegerStruct.ZERO;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.filePosition();
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.BROADCAST_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.BROADCAST_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.BROADCAST_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.BROADCAST_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
