/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.IOStreamStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.LispType;
import jcl.type.StreamType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link AbstractNativeStreamStructImpl} is an abstraction for native stream types.
 */
abstract class AbstractNativeStreamStructImpl extends StreamStructImpl implements IOStreamStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractNativeStreamStructImpl.class);

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
	protected AbstractNativeStreamStructImpl(final StreamType type,
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
}
