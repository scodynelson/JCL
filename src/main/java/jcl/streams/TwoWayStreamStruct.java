/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.types.TwoWayStream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public class TwoWayStreamStruct extends AbstractDualStreamStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7438024800211893512L;

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStruct(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(TwoWayStream.INSTANCE, interactive, inputStream, outputStream);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStream.readByte(eofErrorP, eofValue);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStream.unreadChar(codePoint);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
