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

	/**
	 * {@link ReaderStateMediator} singleton to be used for all ReaderImpl instances.
	 */
	private static final ReaderStateMediator READER_STATE_MEDIATOR = ReaderStateMediatorImpl.INSTANCE;

	/**
	 * The {@link InputStream} the ReaderImpl reads lisp tokens from.
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

	@Override
	public LispStruct read() {
		return read(true, null, true);
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final TokenBuilder tokenBuilder = new TokenBuilder(eofErrorP, eofValue, recursiveP);
		READER_STATE_MEDIATOR.read(this, tokenBuilder);

		return tokenBuilder.getReturnToken();
	}

	@Override
	public ReadPeekResult readChar() {
		return readChar(true, null, true);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	@Override
	public InputStream getInputStream() {
		return inputStream;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
