/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.compiler.real.element.SimpleElement;
import jcl.reader.Reader;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Mediator implementation for {@link Reader} {@link ReaderState} invocations throughout the read process.
 */
@Component
class ReaderStateMediatorImpl implements ReaderStateMediator {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3929831111779653134L;

	/**
	 * {@link ReadReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReadReaderState readReaderState;

	/**
	 * {@link IllegalCharacterReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private IllegalCharacterReaderState illegalCharacterReaderState;

	/**
	 * {@link WhitespaceReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private WhitespaceReaderState whitespaceReaderState;

	/**
	 * {@link MacroCharacterReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private MacroCharacterReaderState macroCharacterReaderState;

	/**
	 * {@link SingleEscapeReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private SingleEscapeReaderState singleEscapeReaderState;

	/**
	 * {@link MultipleEscapeReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private MultipleEscapeReaderState multipleEscapeReaderState;

	/**
	 * {@link ConstituentReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private ConstituentReaderState constituentReaderState;

	/**
	 * {@link EvenMultiEscapeReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private EvenMultiEscapeReaderState evenMultiEscapeReaderState;

	/**
	 * {@link OddMultiEscapeReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private OddMultiEscapeReaderState oddMultiEscapeReaderState;

	/**
	 * {@link TokenAccumulatedReaderState} singleton used by the reader algorithm.
	 */
	@Autowired
	private TokenAccumulatedReaderState tokenAccumulatedReaderState;

	@Override
	public SimpleElement read(final TokenBuilder tokenBuilder) {
		return readReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readIllegalCharacter(final TokenBuilder tokenBuilder) {
		return illegalCharacterReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readWhitespace(final TokenBuilder tokenBuilder) {
		return whitespaceReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readMacroCharacter(final TokenBuilder tokenBuilder) {
		return macroCharacterReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readSingleEscape(final TokenBuilder tokenBuilder) {
		return singleEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readMultipleEscape(final TokenBuilder tokenBuilder) {
		return multipleEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readConstituent(final TokenBuilder tokenBuilder) {
		return constituentReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readEvenMultipleEscape(final TokenBuilder tokenBuilder) {
		return evenMultiEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readOddMultipleEscape(final TokenBuilder tokenBuilder) {
		return oddMultiEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public SimpleElement readTokenAccumulated(final TokenBuilder tokenBuilder) {
		return tokenAccumulatedReaderState.process(tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
