/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;

/**
 * Mediator for {@link Reader} state algorithm invocations throughout the read process.
 */
public interface ReaderStateMediator {

	/**
	 * Read the next {@link LispStruct} token into the provided {@link TokenBuilder} using the provided {@link Reader}.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void read(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading illegal characters.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readIllegalCharacter(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading whitespace type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readWhitespace(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading macro characters.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readMacroCharacter(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading single escape type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readSingleEscape(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading multiple escape type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading constituent type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readConstituent(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading even number multiple escape type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readEvenMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle reading odd number multiple escape type tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readOddMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Handle accumulating read tokens.
	 *
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void readTokenAccumulated(Reader reader, TokenBuilder tokenBuilder);
}
