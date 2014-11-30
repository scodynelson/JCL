/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.Reader;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Mediator implementation for {@link Reader} {@link State} invocations throughout the read process.
 */
class ReaderStateMediatorImpl implements ReaderStateMediator {

	/**
	 * Singleton instance variable.
	 */
	static final ReaderStateMediator INSTANCE = new ReaderStateMediatorImpl();

	/**
	 * Private constructor.
	 */
	private ReaderStateMediatorImpl() {
	}

	@Override
	public void read(final Reader reader, final TokenBuilder tokenBuilder) {
		ReadState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readIllegalCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		IllegalCharacterState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readWhitespace(final Reader reader, final TokenBuilder tokenBuilder) {
		WhitespaceState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readMacroCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		MacroCharacterState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readSingleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		SingleEscapeState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		MultipleEscapeState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readConstituent(final Reader reader, final TokenBuilder tokenBuilder) {
		ConstituentState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readEvenMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		EvenMultiEscapeState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readOddMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		OddMultiEscapeState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readTokenAccumulated(final Reader reader, final TokenBuilder tokenBuilder) {
		TokenAccumulatedState.INSTANCE.process(this, reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
