package jcl.lang.readtable;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;
import jcl.type.LispType;

public class ReaderInputStreamStruct implements InputStreamStruct {

	private final InputStreamStruct inputStreamStruct;

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
	 * The current backquote nesting level when dealing with nested backquotes. This is used to determine proper comma
	 * usage and evaluation.
	 */
	private int backquoteLevel;

	public ReaderInputStreamStruct(final InputStreamStruct inputStreamStruct) {
		this.inputStreamStruct = inputStreamStruct;
	}

	public InputStreamStruct getInputStream() {
		return inputStreamStruct;
	}

	public Map<BigInteger, LispStruct> getSharpEqualFinalTable() {
		return new HashMap<>(sharpEqualFinalTable);
	}

	public Map<BigInteger, SymbolStruct> getSharpEqualTempTable() {
		return new HashMap<>(sharpEqualTempTable);
	}

	public Map<SymbolStruct, LispStruct> getSharpEqualReplTable() {
		return new HashMap<>(sharpEqualReplTable);
	}

	public void clearSharpEqualTables() {
		sharpEqualFinalTable.clear();
		sharpEqualTempTable.clear();
		sharpEqualReplTable.clear();
	}

	public void restoreSharpEqualTables(final Map<BigInteger, LispStruct> newSharpEqualFinalTable,
	                                    final Map<BigInteger, SymbolStruct> newSharpEqualTempTable,
	                                    final Map<SymbolStruct, LispStruct> newSharpEqualReplTable) {
		clearSharpEqualTables();
		sharpEqualFinalTable.putAll(newSharpEqualFinalTable);
		sharpEqualTempTable.putAll(newSharpEqualTempTable);
		sharpEqualReplTable.putAll(newSharpEqualReplTable);
	}

	public int getBackquoteLevel() {
		return backquoteLevel;
	}

	public void incrementBackquoteLevel() {
		backquoteLevel++;
	}

	public void decrementBackquoteLevel() {
		backquoteLevel--;
	}

	@Override
	public boolean close() {
		return inputStreamStruct.close();
	}

	@Override
	public LispType getElementType() {
		return inputStreamStruct.getElementType();
	}

	@Override
	public boolean isInteractive() {
		return inputStreamStruct.isInteractive();
	}

	@Override
	public boolean isOpen() {
		return inputStreamStruct.isOpen();
	}

	@Override
	public boolean isClosed() {
		return inputStreamStruct.isClosed();
	}

	@Override
	public Long fileLength() {
		return inputStreamStruct.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return inputStreamStruct.filePosition(filePosition);
	}

	@Override
	public Long lineNumber() {
		return inputStreamStruct.lineNumber();
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readByte(eofErrorP, eofValue);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStreamStruct.unreadChar(codePoint);
	}

	@Override
	public void clearInput() {
		inputStreamStruct.clearInput();
	}

	@Override
	public boolean listen() {
		return inputStreamStruct.listen();
	}
}
