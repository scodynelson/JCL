package jcl.readtables.reader;

import jcl.LispStruct;
import jcl.readtables.ReadtableStruct;
import jcl.syntax.CaseSpec;
import jcl.syntax.reader.ReadResult;

public interface LispReader {

	LispStruct read();

	LispStruct read(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	ReadResult readChar();

	ReadResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	void unreadChar(int codePoint);

	ReadtableStruct getReadtable();

	CaseSpec getReadtableCase();
}
