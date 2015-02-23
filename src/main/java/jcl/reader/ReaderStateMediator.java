/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.compiler.real.element.SimpleElement;

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
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement read(TokenBuilder tokenBuilder);

	/**
	 * Handle reading illegal characters.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readIllegalCharacter(TokenBuilder tokenBuilder);

	/**
	 * Handle reading whitespace type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readWhitespace(TokenBuilder tokenBuilder);

	/**
	 * Handle reading macro characters.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readMacroCharacter(TokenBuilder tokenBuilder);

	/**
	 * Handle reading single escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readSingleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading constituent type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readConstituent(TokenBuilder tokenBuilder);

	/**
	 * Handle reading even number multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readEvenMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle reading odd number multiple escape type tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readOddMultipleEscape(TokenBuilder tokenBuilder);

	/**
	 * Handle accumulating read tokens.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 *
	 * @return the resulting {@link LispStruct} token after a successful read
	 * TODO: fix Javadoc
	 */
	SimpleElement readTokenAccumulated(TokenBuilder tokenBuilder);
}
