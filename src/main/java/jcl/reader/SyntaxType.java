/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

/**
 * Defines the different syntax types that will determine how the reader parses tokens.
 */
public enum SyntaxType {

	/**
	 * Invalid syntax.
	 */
	INVALID,

	/**
	 * Whitespace syntax.
	 */
	WHITESPACE,

	/**
	 * Non terminating syntax.
	 */
	NON_TERMINATING,

	/**
	 * Constituent syntax.
	 */
	CONSTITUENT,

	/**
	 * Terminating syntax.
	 */
	TERMINATING,

	/**
	 * Single escape syntax.
	 */
	SINGLE_ESCAPE,

	/**
	 * Multiple escape syntax.
	 */
	MULTIPLE_ESCAPE
}
