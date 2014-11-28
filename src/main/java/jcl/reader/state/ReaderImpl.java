/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
public class ReaderImpl implements Reader {

	private static final ReaderStateMediator READER_STATE_MEDIATOR = ReaderStateMediatorImpl.INSTANCE;

	/**
	 * The {@link InputStream} the reader reads lisp tokens from.
	 */
	private final InputStream inputStream;

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} used to read lisp tokens
	 */
	public ReaderImpl(final InputStream inputStream) {
		this.inputStream = inputStream;
	}

	/**
	 * Reads the next {@link LispStruct} from the {@link #inputStream}. This calls the overloaded {@link #read(boolean,
	 * LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and {@code recursiveP} as
	 * true.
	 *
	 * @return the next {@link LispStruct} from the {@link #inputStream}.
	 */
	@Override
	public LispStruct read() {
		return read(true, null, true);
	}

	/**
	 * Reads the next {@link LispStruct} from the {@link #inputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link #inputStream}.
	 */
	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final TokenBuilder tokenBuilder = new TokenBuilder(eofErrorP, eofValue, recursiveP);
		READER_STATE_MEDIATOR.read(this, tokenBuilder);

		return tokenBuilder.getReturnToken();
	}

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link #inputStream}. This calls the overloaded {@link
	 * #readChar(boolean, LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and
	 * {@code recursiveP} as true.
	 *
	 * @return the next {@link ReadPeekResult} from the {@link #inputStream}.
	 */
	@Override
	public ReadPeekResult readChar() {
		return readChar(true, null, true);
	}

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link #inputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link ReadPeekResult} from the {@link #inputStream}.
	 */
	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	/**
	 * Un-reads the provided {@code codePoint} value from (or really back into) the {@link #inputStream}.
	 *
	 * @param codePoint
	 * 		the value to un-read from (or really back into) the {@link #inputStream}
	 */
	@Override
	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	/**
	 * Gets the {@link #inputStream} for the JCL Reader instance.
	 *
	 * @return the {@link #inputStream} for the JCL Reader instance.
	 */
	@Override
	public InputStream getInputStream() {
		return inputStream;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
