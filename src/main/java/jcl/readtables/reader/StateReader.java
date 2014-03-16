package jcl.readtables.reader;

import jcl.readtables.ReadtableStruct;
import jcl.syntax.CaseSpec;

public interface StateReader extends LispReader {

	ReadtableStruct getReadtable();

	CaseSpec getReadtableCase();
}
