/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.BroadcastStream;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * The {@link BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public class BroadcastStreamStruct extends StreamStruct implements OutputStream {

	/**
	 * This {@link OutputStream}s in the BroadcastStreamStruct.
	 */
	private final Deque<OutputStream> outputStreams;

	/**
	 * Public constructor.
	 *
	 * @param outputStreams
	 * 		the {@link OutputStream}s to create a BroadcastStreamStruct from
	 */
	public BroadcastStreamStruct(final Deque<OutputStream> outputStreams) {
		this(false, outputStreams);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param outputStreams
	 * 		the {@link OutputStream}s to create a BroadcastStreamStruct from
	 */
	public BroadcastStreamStruct(final boolean interactive, final Deque<OutputStream> outputStreams) {
		super(BroadcastStream.INSTANCE, null, null, interactive, getElementType2(outputStreams));
		this.outputStreams = new ArrayDeque<>(outputStreams);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param outputStreams
	 * 		the {@link OutputStream}s to create a BroadcastStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType2(final Deque<OutputStream> outputStreams) {
		if (outputStreams == null) {
			throw new StreamErrorException("Provided Output Stream List must not be null.");
		}
		return getElementType3(outputStreams);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param outputStreams
	 * 		the {@link OutputStream}s to create a BroadcastStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType3(final Deque<OutputStream> outputStreams) {
		if (outputStreams.isEmpty()) {
			return T.INSTANCE;
		}

		final OutputStream last = outputStreams.getLast();
		return last.getElementType();
	}

	@Override
	public LispType getType() {
		return BroadcastStream.INSTANCE;
	}

	@Override
	public void writeChar(final int aChar) {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeChar(aChar);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeByte(aByte);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeString(outputString, start, end);
		}
	}

	@Override
	public void clearOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.clearOutput();
		}
	}

	@Override
	public void finishOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.finishOutput();
		}
	}

	@Override
	public void forceOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.forceOutput();
		}
	}

	@Override
	public LispType getElementType() {
		return getElementType3(outputStreams);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	@Override
	public Long fileLength() {
		if (outputStreams.isEmpty()) {
			return 0L;
		}

		final OutputStream last = outputStreams.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (outputStreams.isEmpty()) {
			return 0L;
		}

		final OutputStream last = outputStreams.getLast();
		return last.filePosition(filePosition);
	}
}
