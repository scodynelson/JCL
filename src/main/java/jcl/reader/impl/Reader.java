package jcl.reader.impl;

import jcl.LispStruct;
import jcl.structs.readtables.ReadtableStruct;
import jcl.structs.readtables.ReadtableVariable;
import jcl.reader.syntax.TokenBuilder;
import jcl.structs.streams.InputStream;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import jcl.syntax.SyntaxType;
import jcl.syntax.reader.ReadResult;

public class Reader {

	private final InputStream inputStream;
	private final ReadtableStruct readtable;

	public Reader(final InputStream inputStream) {
		this(inputStream, ReadtableVariable.INSTANCE.getValue());
	}

	public Reader(final InputStream inputStream, final ReadtableStruct readtable) {
		this.inputStream = inputStream;
		this.readtable = readtable;
	}

	public LispStruct read() {
		return read(true, null, true);
	}

	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final Reader reader = new Reader(inputStream);

		final TokenBuilder tokenBuilder = new TokenBuilder(eofErrorP, eofValue, recursiveP);
		State.getInitialState().process(reader, tokenBuilder);

		return tokenBuilder.getReturnToken();
	}

	public ReadResult readChar() {
		return readChar(true, null, true);
	}

	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	public ReadtableStruct getReadtable() {
		return readtable;
	}

	public CaseSpec getReadtableCase() {
		return readtable.getReadtableCase();
	}

	public SyntaxType getSyntaxType(final int codePoint) {
		return readtable.getSyntaxType(codePoint);
	}

	public AttributeType getAttributeType(final int codePoint) {
		return readtable.getAttributeType(codePoint);
	}
}
