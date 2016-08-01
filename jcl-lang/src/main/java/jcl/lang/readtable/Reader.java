/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.readtable;

import java.math.BigInteger;
import java.util.Map;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.stream.InputStreamStruct;
import jcl.lang.stream.ReadPeekResult;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
public interface Reader {

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStreamStruct}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link InputStreamStruct}
	 */
	LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads the next {@link LispStruct} from the {@link InputStreamStruct}, making sure to preserve any whitespace
	 * characters after the {@link LispStruct} token is read.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link InputStreamStruct}
	 */
	LispStruct readPreservingWhitespace(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link InputStreamStruct}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link ReadPeekResult} from the {@link InputStreamStruct}
	 */
	ReadPeekResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Un-reads the provided {@code codePoint} value from (or really back into) the {@link InputStreamStruct}.
	 *
	 * @param codePoint
	 * 		the value to un-read from (or really back into) the {@link InputStreamStruct}
	 */
	void unreadChar(int codePoint);

	/**
	 * Gets the {@link InputStreamStruct} for the JCL Reader instance.
	 *
	 * @return the {@link InputStreamStruct} for the JCL Reader instance
	 */
	InputStreamStruct getInputStreamStruct();

	/**
	 * Gets the {@link Map} #n= labels to their assigned {@link LispStruct}s for the JCL Reader instance.
	 *
	 * @return the {@link Map} #n= labels to their assigned {@link LispStruct}s for the JCL Reader instance
	 */
	Map<BigInteger, LispStruct> getSharpEqualFinalTable();

	/**
	 * Gets the {@link Map} #n= labels to their temporary {@link SymbolStruct} tags for the JCL Reader instance.
	 *
	 * @return the {@link Map} #n= labels to their temporary {@link SymbolStruct} tags for the JCL Reader instance
	 */
	Map<BigInteger, SymbolStruct> getSharpEqualTempTable();

	/**
	 * Gets the {@link Map} the temporary {@link SymbolStruct} tags for #n= labels to their assigned {@link
	 * LispStruct}s
	 * for the JCL Reader instance.
	 *
	 * @return the {@link Map} the temporary {@link SymbolStruct} tags for #n= labels to their assigned {@link
	 * LispStruct}s for the JCL Reader instance
	 */
	Map<SymbolStruct, LispStruct> getSharpEqualReplTable();

	/**
	 * Gets the current backquote level for the JCL Reader instance.
	 *
	 * @return the current backquote level for the JCL Reader instance
	 */
	int getBackquoteLevel();

	/**
	 * Increments the current backquote level for the JCL Reader instance.
	 */
	void incrementBackquoteLevel();

	/**
	 * Decrements the current backquote level for the JCL Reader instance.
	 */
	void decrementBackquoteLevel();
}
