package jcl.readtables.reader.impl.states;

import jcl.readtables.ReadtableStruct;
import jcl.readtables.reader.LispReader;
import jcl.syntax.CaseSpec;

public interface StateReader extends LispReader {

	ReadtableStruct getReadtable();

	CaseSpec getReadtableCase();
}
