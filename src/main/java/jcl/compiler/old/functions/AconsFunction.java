package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;

public class AconsFunction {
	public static final AconsFunction FUNCTION = new AconsFunction();

	public ConsStruct funcall(LispStruct key, LispStruct datum, ListStruct aList) {
		return new ConsStruct(new ConsStruct(key, datum), aList);
	}
}