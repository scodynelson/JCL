/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.Reader;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Mediator implementation for {@link Reader} {@link ReaderState} invocations throughout the read process.
 */
@Component
class ReaderStateMediatorImpl implements ReaderStateMediator {

	@Autowired
	private ReadReaderState readReaderState;

	@Autowired
	private IllegalCharacterReaderState illegalCharacterReaderState;

	@Autowired
	private WhitespaceReaderState whitespaceReaderState;

	@Autowired
	private MacroCharacterReaderState macroCharacterReaderState;

	@Autowired
	private SingleEscapeReaderState singleEscapeReaderState;

	@Autowired
	private MultipleEscapeReaderState multipleEscapeReaderState;

	@Autowired
	private ConstituentReaderState constituentReaderState;

	@Autowired
	private EvenMultiEscapeReaderState evenMultiEscapeReaderState;

	@Autowired
	private OddMultiEscapeReaderState oddMultiEscapeReaderState;

	@Autowired
	private TokenAccumulatedReaderState tokenAccumulatedReaderState;

	@Override
	public void read(final Reader reader, final TokenBuilder tokenBuilder) {
		readReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readIllegalCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		illegalCharacterReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readWhitespace(final Reader reader, final TokenBuilder tokenBuilder) {
		whitespaceReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readMacroCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		macroCharacterReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readSingleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		singleEscapeReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		multipleEscapeReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readConstituent(final Reader reader, final TokenBuilder tokenBuilder) {
		constituentReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readEvenMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		evenMultiEscapeReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readOddMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		oddMultiEscapeReaderState.process(reader, tokenBuilder);
	}

	@Override
	public void readTokenAccumulated(final Reader reader, final TokenBuilder tokenBuilder) {
		tokenAccumulatedReaderState.process(reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
