package jcl.readtables.reader;

import jcl.LispStruct;
import jcl.streams.InputStream;
import jcl.syntax.reader.ReadResult;

public abstract class LispReader {

	public abstract LispStruct read();

	public abstract LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	public abstract ReadResult readChar();

	public abstract ReadResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	public abstract void unreadChar(int codePoint);

	public static LispReader getReader(final InputStream inputStream) {
		return new StateReader(inputStream);
	}
}
