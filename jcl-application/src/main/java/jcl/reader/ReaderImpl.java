/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.ReadPeekResult;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class ReaderImpl implements Reader {

	/**
	 * The {@link InputStream} the ReaderImpl reads lisp tokens from.
	 */
	private final InputStream inputStream;

	/**
	 * Map containing the number argument to #= to parsed {@link LispStruct}s produced by the #= reader macro function.
	 */
	private final Map<BigInteger, LispStruct> sharpEqualFinalTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the number argument of #= to a temporary {@link UUID} tag value to handle {@link LispStruct}s not
	 * yet parsed by the reader.
	 */
	private final Map<BigInteger, SymbolStruct> sharpEqualTempTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the temporary {@link UUID} tag value to a {@link LispStruct} that has been parsed by the reader,
	 * but may have yet to return to the top level of the #= parse.
	 */
	private final Map<SymbolStruct, LispStruct> sharpEqualReplTable = new ConcurrentHashMap<>();

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	/**
	 * The current backquote nesting level when dealing with nested backquotes. This is used to determine proper comma
	 * usage and evaluation.
	 */
	private int backquoteLevel;

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} used to read lisp tokens
	 */
	ReaderImpl(final InputStream inputStream) {
		this.inputStream = inputStream;
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final LispStruct token = readPreservingWhitespace(eofErrorP, eofValue, recursiveP);

		final ReadPeekResult possibleWhitespace = readChar(false, eofValue, false);
		final Integer codePoint = possibleWhitespace.getResult();
		if (!possibleWhitespace.isEof() && (!Character.isWhitespace(codePoint) || recursiveP)) {
			unreadChar(codePoint);
		}

		return token;
	}

	@Override
	public LispStruct readPreservingWhitespace(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (recursiveP) {
			final TokenBuilder tokenBuilder = new TokenBuilder(this, eofErrorP, eofValue);
			return readerStateMediator.read(tokenBuilder);
		}

		final Map<BigInteger, LispStruct> tempSharpEqualFinalTable = new HashMap<>(sharpEqualFinalTable);
		final Map<BigInteger, SymbolStruct> tempSharpEqualTempTable = new HashMap<>(sharpEqualTempTable);
		final Map<SymbolStruct, LispStruct> tempSharpEqualReplTable = new HashMap<>(sharpEqualReplTable);

		try {
			sharpEqualFinalTable.clear();
			sharpEqualTempTable.clear();
			sharpEqualReplTable.clear();

			return readPreservingWhitespace(eofErrorP, eofValue, true);
		} finally {
			// Clear them back out.
			sharpEqualFinalTable.clear();
			sharpEqualTempTable.clear();
			sharpEqualReplTable.clear();

			// Restore their values.
			sharpEqualFinalTable.putAll(tempSharpEqualFinalTable);
			sharpEqualTempTable.putAll(tempSharpEqualTempTable);
			sharpEqualReplTable.putAll(tempSharpEqualReplTable);
		}
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	@Override
	public InputStream getInputStream() {
		return inputStream;
	}

	@Override
	public Map<BigInteger, LispStruct> getSharpEqualFinalTable() {
		return sharpEqualFinalTable;
	}

	@Override
	public Map<BigInteger, SymbolStruct> getSharpEqualTempTable() {
		return sharpEqualTempTable;
	}

	@Override
	public Map<SymbolStruct, LispStruct> getSharpEqualReplTable() {
		return sharpEqualReplTable;
	}

	@Override
	public int getBackquoteLevel() {
		return backquoteLevel;
	}

	@Override
	public void incrementBackquoteLevel() {
		backquoteLevel++;
	}

	@Override
	public void decrementBackquoteLevel() {
		backquoteLevel--;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(inputStream)
		                            .append(sharpEqualFinalTable)
		                            .append(sharpEqualTempTable)
		                            .append(sharpEqualReplTable)
		                            .append(readerStateMediator)
		                            .append(backquoteLevel)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ReaderImpl rhs = (ReaderImpl) obj;
		return new EqualsBuilder().append(inputStream, rhs.inputStream)
		                          .append(sharpEqualFinalTable, rhs.sharpEqualFinalTable)
		                          .append(sharpEqualTempTable, rhs.sharpEqualTempTable)
		                          .append(sharpEqualReplTable, rhs.sharpEqualReplTable)
		                          .append(readerStateMediator, rhs.readerStateMediator)
		                          .append(backquoteLevel, rhs.backquoteLevel)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(inputStream)
		                                                                .append(sharpEqualFinalTable)
		                                                                .append(sharpEqualTempTable)
		                                                                .append(sharpEqualReplTable)
		                                                                .append(readerStateMediator)
		                                                                .append(backquoteLevel)
		                                                                .toString();
	}
}
