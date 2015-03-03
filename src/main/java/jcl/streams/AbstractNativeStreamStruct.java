/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispType;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.Stream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link AbstractNativeStreamStruct} is an abstraction for native stream types.
 */
abstract class AbstractNativeStreamStruct extends StreamStruct implements InputStream, OutputStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4334429877946908390L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractNativeStreamStruct.class);

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the stream object
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the stream elementType
	 */
	AbstractNativeStreamStruct(final Stream type,
	                           final boolean interactive, final LispType elementType) {
		super(type, null, null, interactive, elementType);
	}

	@Override
	public boolean listen() {
		try {
			final ReadPeekResult peekResult = peekChar(PeekType.NIL_PEEK_TYPE, false, null, false);
			return !peekResult.isEof();
		} catch (final EndOfFileException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return false;
		} catch (final StreamErrorException see) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Stream error occurred.", see);
			}
			return false;
		}
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
