/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.Reader;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ReaderStateMediatorImpl implements ReaderStateMediator {

	/**
	 * Singleton instance variable.
	 */
	public static final ReaderStateMediator INSTANCE = new ReaderStateMediatorImpl();

	private static final State READ_STATE = ReadState.INSTANCE;
	private static final State ILLEGAL_CHARACTER_STATE = IllegalCharacterState.INSTANCE;
	private static final State WHITESPACE_STATE = WhitespaceState.INSTANCE;
	private static final State MACRO_CHARACTER_STATE = MacroCharacterState.INSTANCE;
	private static final State SINGLE_ESCAPE_STATE = SingleEscapeState.INSTANCE;
	private static final State MULTIPLE_ESCAPE_STATE = MultipleEscapeState.INSTANCE;
	private static final State CONSTITUENT_STATE = ConstituentState.INSTANCE;
	private static final State EVEN_MULTIPLE_ESCAPE_STATE = EvenMultiEscapeState.INSTANCE;
	private static final State ODD_MULTIPLE_ESCAPE_STATE = OddMultiEscapeState.INSTANCE;
	private static final State TOKEN_ACCUMULATED_STATE = TokenAccumulatedState.INSTANCE;

	/**
	 * Private constructor.
	 */
	private ReaderStateMediatorImpl() {
	}

	@Override
	public void read(final Reader reader, final TokenBuilder tokenBuilder) {
		READ_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readIllegalCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		ILLEGAL_CHARACTER_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readWhitespace(final Reader reader, final TokenBuilder tokenBuilder) {
		WHITESPACE_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readMacroCharacter(final Reader reader, final TokenBuilder tokenBuilder) {
		MACRO_CHARACTER_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readSingleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		SINGLE_ESCAPE_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		MULTIPLE_ESCAPE_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readConstituent(final Reader reader, final TokenBuilder tokenBuilder) {
		CONSTITUENT_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readEvenMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		EVEN_MULTIPLE_ESCAPE_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readOddMultipleEscape(final Reader reader, final TokenBuilder tokenBuilder) {
		ODD_MULTIPLE_ESCAPE_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public void readTokenAccumulated(final Reader reader, final TokenBuilder tokenBuilder) {
		TOKEN_ACCUMULATED_STATE.process(this, reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
