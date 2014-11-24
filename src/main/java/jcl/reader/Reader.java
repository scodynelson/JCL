/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.symbols.variables.Variable;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
public class Reader {

	/**
	 * The {@link InputStream} the reader reads lisp tokens from.
	 */
	private final InputStream inputStream;

	/**
	 * The {@link ReadtableStruct} the reader uses when reading lisp tokens from the {@link #inputStream}.
	 */
	private final ReadtableStruct readtable;

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} used to read lisp tokens
	 */
	public Reader(final InputStream inputStream) {
		this(inputStream, Variable.READTABLE.getValue());
	}

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} used to read lisp tokens
	 * @param readtable
	 * 		the {@link ReadtableStruct} to use for parsing lisp tokens into {@link LispStruct}s
	 */
	public Reader(final InputStream inputStream, final ReadtableStruct readtable) {
		this.inputStream = inputStream;
		this.readtable = readtable;
	}

	/**
	 * Reads the next {@link LispStruct} from the {@link #inputStream}. This calls the overloaded {@link #read(boolean,
	 * LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and {@code recursiveP} as
	 * true.
	 *
	 * @return the next {@link LispStruct} from the {@link #inputStream}.
	 */
	public LispStruct read() {
		return read(true, null, true);
	}

	/**
	 * Reads the next {@link LispStruct} from the {@link #inputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link LispStruct} from the {@link #inputStream}.
	 */
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final TokenBuilder tokenBuilder = new TokenBuilder(eofErrorP, eofValue, recursiveP);
		ReadState.INSTANCE.process(this, tokenBuilder);

		return tokenBuilder.getReturnToken();
	}

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link #inputStream}. This calls the overloaded {@link
	 * #readChar(boolean, LispStruct, boolean)} method with {@code eofErrorP} as true, {@code eofValue} as null, and
	 * {@code recursiveP} as true.
	 *
	 * @return the next {@link ReadPeekResult} from the {@link #inputStream}.
	 */
	public ReadPeekResult readChar() {
		return readChar(true, null, true);
	}

	/**
	 * Reads the next {@link ReadPeekResult} from the {@link #inputStream}.
	 *
	 * @param eofErrorP
	 * 		whether or not to throw an error when an End-Of-File is reached
	 * @param eofValue
	 * 		the value to return if an End-Of-File is reached and an error is not to be thrown
	 * @param recursiveP
	 * 		whether or not to recursively read tokens
	 *
	 * @return the next {@link ReadPeekResult} from the {@link #inputStream}.
	 */
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	/**
	 * Un-reads the provided {@code codePoint} value from (or really back into) the {@link #inputStream}.
	 *
	 * @param codePoint
	 * 		the value to un-read from (or really back into) the {@link #inputStream}
	 */
	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	/**
	 * Gets the {@link #inputStream} for the JCL Reader instance.
	 *
	 * @return the {@link #inputStream} for the JCL Reader instance.
	 */
	public InputStream getInputStream() {
		return inputStream;
	}

	/**
	 * Gets the {@link #readtable} for the JCL Reader instance.
	 *
	 * @return the {@link #readtable} for the JCL Reader instance.
	 */
	public ReadtableStruct getReadtable() {
		return readtable;
	}

	/**
	 * Gets the {@link CaseSpec} from the {@link #readtable} for the JCL Reader instance.
	 *
	 * @return the {@link CaseSpec} from the {@link #readtable} for the JCL Reader instance.
	 */
	public CaseSpec getReadtableCase() {
		return readtable.getReadtableCase();
	}

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} from the {@link #readtable} for the JCL Reader
	 * instance.
	 *
	 * @param codePoint
	 * 		the value to get the {@link SyntaxType} for from the {@link #readtable}
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} from the {@link #readtable}
	 */
	public SyntaxType getSyntaxType(final int codePoint) {
		return readtable.getSyntaxType(codePoint);
	}

	/**
	 * Gets the {@link AttributeType} for the provided {@code codePoint} from the {@link #readtable} for the JCL Reader
	 * instance.
	 *
	 * @param codePoint
	 * 		the value to get the {@link AttributeType} for from the {@link #readtable}
	 *
	 * @return the {@link AttributeType} for the provided {@code codePoint} from the {@link #readtable}
	 */
	public AttributeType getAttributeType(final int codePoint) {
		return readtable.getAttributeType(codePoint);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
