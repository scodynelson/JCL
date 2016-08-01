/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.lang.OutputStreamStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.type.BroadcastStreamType;
import jcl.type.LispType;
import jcl.type.TType;

/**
 * The {@link BroadcastStreamStructImpl} is the object representation of a Lisp 'broadcast-stream' type.
 */
public final class BroadcastStreamStructImpl extends StreamStructImpl implements OutputStreamStruct {

	/**
	 * This {@link OutputStreamStruct}s in the BroadcastStreamStruct.
	 */
	private final Deque<OutputStreamStruct> outputStreamStructs;

	/**
	 * Public constructor.
	 *
	 * @param outputStreamStructs
	 * 		the {@link OutputStreamStruct}s to create a BroadcastStreamStruct from
	 */
	private BroadcastStreamStructImpl(final Deque<OutputStreamStruct> outputStreamStructs) {
		this(false, outputStreamStructs);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param outputStreamStructs
	 * 		the {@link OutputStreamStruct}s to create a BroadcastStreamStruct from
	 */
	private BroadcastStreamStructImpl(final boolean interactive, final Deque<OutputStreamStruct> outputStreamStructs) {
		super(BroadcastStreamType.INSTANCE, null, null, interactive, getElementType2(outputStreamStructs));
		this.outputStreamStructs = new ArrayDeque<>(outputStreamStructs);
	}

	public static BroadcastStreamStructImpl valueOf(final Deque<OutputStreamStruct> outputStreamStructs) {
		return new BroadcastStreamStructImpl(outputStreamStructs);
	}

	public static BroadcastStreamStructImpl valueOf(final boolean interactive, final Deque<OutputStreamStruct> outputStreamStructs) {
		return new BroadcastStreamStructImpl(interactive, outputStreamStructs);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param outputStreamStructs
	 * 		the {@link OutputStreamStruct}s to create a BroadcastStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType2(final Deque<OutputStreamStruct> outputStreamStructs) {
		if (outputStreamStructs == null) {
			throw new ErrorException("Provided Output Stream List must not be null.");
		}
		return getElementType3(outputStreamStructs);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param outputStreamStructs
	 * 		the {@link OutputStreamStruct}s to create a BroadcastStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType3(final Deque<OutputStreamStruct> outputStreamStructs) {
		if (outputStreamStructs.isEmpty()) {
			return TType.INSTANCE;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.getElementType();
	}

	public Deque<OutputStreamStruct> getOutputStreamStructs() {
		return outputStreamStructs;
	}

	@Override
	public void writeChar(final int aChar) {
		outputStreamStructs.forEach(e -> e.writeChar(aChar));
	}

	@Override
	public void writeByte(final int aByte) {
		outputStreamStructs.forEach(e -> e.writeChar(aByte));
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		outputStreamStructs.forEach(e -> e.writeString(outputString, start, end));
	}

	@Override
	public void clearOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::clearOutput);
	}

	@Override
	public void finishOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::finishOutput);
	}

	@Override
	public void forceOutput() {
		outputStreamStructs.forEach(OutputStreamStruct::forceOutput);
	}

	@Override
	public boolean isStartOfLine() {
		return outputStreamStructs.stream().map(OutputStreamStruct::isStartOfLine).reduce(false, Boolean::logicalAnd);
	}

	@Override
	public LispType getElementType() {
		return getElementType3(outputStreamStructs);
	}

	@Override
	public Long fileLength() {
		if (outputStreamStructs.isEmpty()) {
			return 0L;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (outputStreamStructs.isEmpty()) {
			return 0L;
		}

		final OutputStreamStruct last = outputStreamStructs.getLast();
		return last.filePosition(filePosition);
	}
}
