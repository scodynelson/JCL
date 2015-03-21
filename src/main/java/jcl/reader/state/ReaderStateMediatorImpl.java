/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
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
	public LispStruct read(final TokenBuilder tokenBuilder) {
		return readReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readIllegalCharacter(final TokenBuilder tokenBuilder) {
		return illegalCharacterReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readWhitespace(final TokenBuilder tokenBuilder) {
		return whitespaceReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readMacroCharacter(final TokenBuilder tokenBuilder) {
		return macroCharacterReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readSingleEscape(final TokenBuilder tokenBuilder) {
		return singleEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readMultipleEscape(final TokenBuilder tokenBuilder) {
		return multipleEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readConstituent(final TokenBuilder tokenBuilder) {
		return constituentReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readEvenMultipleEscape(final TokenBuilder tokenBuilder) {
		return evenMultiEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readOddMultipleEscape(final TokenBuilder tokenBuilder) {
		return oddMultiEscapeReaderState.process(tokenBuilder);
	}

	@Override
	public LispStruct readTokenAccumulated(final TokenBuilder tokenBuilder) {
		return tokenAccumulatedReaderState.process(tokenBuilder);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
