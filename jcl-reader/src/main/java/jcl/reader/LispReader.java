package jcl.reader;

import jcl.reader.state.StateReader;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.streams.InputStream;
import jcl.structs.streams.ReadResult;

public abstract class LispReader {

	public abstract LispStruct read() throws ReaderErrorException;

	public abstract LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP) throws ReaderErrorException;

	public abstract ReadResult readChar() throws ReaderErrorException;

	public abstract ReadResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP) throws ReaderErrorException;

	public abstract void unreadChar(int codePoint) throws ReaderErrorException;

	public static LispReader getReader(final InputStream inputStream) {
		return new StateReader(inputStream);
	}
}
