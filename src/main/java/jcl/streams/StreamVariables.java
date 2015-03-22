/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Variable;

public interface StreamVariables {

	Variable<?> DEBUG_IO = new Variable<>("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> ERROR_OUTPUT = new Variable<>("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> QUERY_IO = new Variable<>("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> STANDARD_INPUT = new Variable<>("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> STANDARD_OUTPUT = new Variable<>("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> TRACE_OUTPUT = new Variable<>("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	Variable<?> TERMINAL_IO = new Variable<>("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP, null);
}
