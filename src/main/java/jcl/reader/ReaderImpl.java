/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.compiler.real.element.SimpleElement;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class ReaderImpl implements Reader {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7380620097058028927L;

	/**
	 * The {@link InputStream} the ReaderImpl reads lisp tokens from.
	 */
	private final InputStream inputStream;

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} used to read lisp tokens
	 */
	ReaderImpl(final InputStream inputStream) {
		this.inputStream = inputStream;
	}

	@Override
	public SimpleElement read() {
		return read(true, null, true);
	}

	@Override
	public SimpleElement read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final TokenBuilder tokenBuilder = new TokenBuilder(this, eofErrorP, eofValue, recursiveP);
		return readerStateMediator.read(tokenBuilder);
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
