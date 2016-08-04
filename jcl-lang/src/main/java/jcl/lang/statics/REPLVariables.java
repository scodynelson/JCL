/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.internal.VariableStructImpl;

public interface REPLVariables {

	VariableStructImpl<LispStruct> DASH = VariableStructImpl.valueOf("-", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> PLUS = VariableStructImpl.valueOf("+", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> PLUS_PLUS = VariableStructImpl.valueOf("++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> PLUS_PLUS_PLUS = VariableStructImpl.valueOf("+++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> STAR = VariableStructImpl.valueOf("*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> STAR_STAR = VariableStructImpl.valueOf("**", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<LispStruct> STAR_STAR_STAR = VariableStructImpl.valueOf("***", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<ListStruct> SLASH = VariableStructImpl.valueOf("/", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<ListStruct> SLASH_SLASH = VariableStructImpl.valueOf("//", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<ListStruct> SLASH_SLASH_SLASH = VariableStructImpl.valueOf("///", GlobalPackageStruct.COMMON_LISP, null);
}
