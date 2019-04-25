/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.IOStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;
import lombok.extern.slf4j.Slf4j;

/**
 * The {@link AbstractNativeStreamStructImpl} is an abstraction for native stream types.
 */
@Slf4j
abstract class AbstractNativeStreamStructImpl extends StreamStructImpl implements IOStreamStruct {

	/**
	 * Protected constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the stream elementType
	 */
	protected AbstractNativeStreamStructImpl(final boolean interactive, final LispStruct elementType) {
		super(interactive, elementType);
	}

	@Override
	public boolean listen() {
		try {
			final ReadPeekResult peekResult = peekChar(PeekType.NIL_PEEK_TYPE, false, null, false);
			return !peekResult.isEof();
		} catch (final EndOfFileException eofe) {
			log.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return false;
		} catch (final StreamErrorException see) {
			log.warn("Stream error occurred.", see);
			return false;
		}
	}
}
