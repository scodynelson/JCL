/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.repl;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface REPLVariables {

	VariableStruct<LispStruct> DASH = new VariableStruct<>("-", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS = new VariableStruct<>("+", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS_PLUS = new VariableStruct<>("++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> PLUS_PLUS_PLUS = new VariableStruct<>("+++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR = new VariableStruct<>("*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR_STAR = new VariableStruct<>("**", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<LispStruct> STAR_STAR_STAR = new VariableStruct<>("***", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH = new VariableStruct<>("/", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH_SLASH = new VariableStruct<>("//", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<ListStruct> SLASH_SLASH_SLASH = new VariableStruct<>("///", GlobalPackageStruct.COMMON_LISP, null);
}
