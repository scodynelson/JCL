package jcl.compiler.old;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;

public class WrapInLambda {

	/**
	 * @param obj the object to put inside the blank lambda
	 * @return the list (lambda () obj)
	 */
	public ListStruct funcall(LispStruct obj) {
		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("LAMBDA").getSymbolStruct(), NullStruct.INSTANCE, obj);
	}
}
