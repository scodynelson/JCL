package jcl.readtables;

import jcl.LispStruct;
import jcl.syntax.reader.ReadResult;

public interface LispReader {

	LispStruct read();

	LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	ReadResult readChar();

	ReadResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	void unreadChar(int codePoint);
}
