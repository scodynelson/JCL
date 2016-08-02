/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.VariableStruct;

public interface REPLVariables {

	VariableStruct<LispStruct> DASH = VariableStruct.valueOf("-", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS = VariableStruct.valueOf("+", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS_PLUS = VariableStruct.valueOf("++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS_PLUS_PLUS = VariableStruct.valueOf("+++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR = VariableStruct.valueOf("*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR_STAR = VariableStruct.valueOf("**", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR_STAR_STAR = VariableStruct.valueOf("***", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH = VariableStruct.valueOf("/", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH_SLASH = VariableStruct.valueOf("//", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH_SLASH_SLASH = VariableStruct.valueOf("///", GlobalPackageStruct.COMMON_LISP, null);
}
