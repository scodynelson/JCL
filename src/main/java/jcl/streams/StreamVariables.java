/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface StreamVariables {

	VariableStruct<?> DEBUG_IO = new VariableStruct<>("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> ERROR_OUTPUT = new VariableStruct<>("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> QUERY_IO = new VariableStruct<>("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> STANDARD_INPUT = new VariableStruct<>("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> STANDARD_OUTPUT = new VariableStruct<>("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> TRACE_OUTPUT = new VariableStruct<>("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> TERMINAL_IO = new VariableStruct<>("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP, null);
}
