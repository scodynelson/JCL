/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.VariableStruct;

public interface StreamVariables {

	VariableStruct<TwoWayStreamStructImpl> TERMINAL_IO = VariableStruct.valueOf("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP,
	                                                                            TwoWayStreamStructImpl.valueOf(true, EmptyStreamStructImpl.INSTANCE, EmptyStreamStructImpl.INSTANCE));

	VariableStruct<IOStreamStruct> DEBUG_IO = VariableStruct.valueOf("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStruct<OutputStreamStruct> ERROR_OUTPUT = VariableStruct.valueOf("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStruct<IOStreamStruct> QUERY_IO = VariableStruct.valueOf("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStruct<InputStreamStruct> STANDARD_INPUT = VariableStruct.valueOf("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStruct<OutputStreamStruct> STANDARD_OUTPUT = VariableStruct.valueOf("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStruct<OutputStreamStruct> TRACE_OUTPUT = VariableStruct.valueOf("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
}
