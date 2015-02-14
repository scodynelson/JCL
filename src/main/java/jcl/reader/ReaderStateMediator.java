/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;

import java.io.Serializable;

/**
 * Mediator for {@link Reader} state algorithm invocations throughout the read process.
 */
public interface ReaderStateMediator extends Serializable {

	/**
	 * Read the next {@link LispStruct} token into the provided {@link TokenBuilder} using the provided {@link Reader}.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void read(TokenBuilder tokenBuilder);

	/**
	 * Handle reading illegal characters.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readIllegalCharacter(TokenBuilder tokenBuilder);

	/**
	 * Handle reading whitespace type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readWhitespace(TokenBuilder tokenBuilder);

	/**
	 * Handle reading macro characters.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readMacroCharacter(TokenBuilder tokenBuilder);

	/**
	 * Handle reading single escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readSingleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading constituent type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readConstituent(TokenBuilder tokenBuilder);

	/**
	 * Handle reading even number multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readEvenMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading odd number multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readOddMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle accumulating read tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readTokenAccumulated(TokenBuilder tokenBuilder);
}
