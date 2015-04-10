/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.LispType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.BroadcastStreamType;
import jcl.types.TType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public class BroadcastStreamStruct extends StreamStruct implements OutputStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 9133930758245275144L;

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
		super(BroadcastStreamType.INSTANCE, null, null, interactive, getElementType2(outputStreams));
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
			return TType.INSTANCE;
		}

		final OutputStream last = outputStreams.getLast();
		return last.getElementType();
	}

	@Override
	public void writeChar(final int aChar) {
		outputStreams.forEach(e -> e.writeChar(aChar));
	}

	@Override
	public void writeByte(final int aByte) {
		outputStreams.forEach(e -> e.writeChar(aByte));
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		outputStreams.forEach(e -> e.writeString(outputString, start, end));
	}

	@Override
	public void clearOutput() {
		outputStreams.forEach(OutputStream::clearOutput);
	}

	@Override
	public void finishOutput() {
		outputStreams.forEach(OutputStream::finishOutput);
	}

	@Override
	public void forceOutput() {
		outputStreams.forEach(OutputStream::forceOutput);
	}

	@Override
	public LispType getElementType() {
		return getElementType3(outputStreams);
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(outputStreams)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final BroadcastStreamStruct rhs = (BroadcastStreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(outputStreams, rhs.outputStreams)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(outputStreams)
		                                                                .toString();
	}
}
