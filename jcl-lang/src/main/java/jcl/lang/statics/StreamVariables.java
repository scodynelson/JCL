/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IOStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.internal.stream.EmptyStreamStructImpl;
import jcl.lang.internal.stream.SynonymStreamStructImpl;
import jcl.lang.internal.stream.TwoWayStreamStructImpl;

public interface StreamVariables {

	VariableStructImpl<TwoWayStreamStructImpl> TERMINAL_IO = VariableStructImpl.valueOf("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP,
	                                                                                    TwoWayStreamStructImpl.valueOf(true, EmptyStreamStructImpl.INSTANCE, EmptyStreamStructImpl.INSTANCE));

	VariableStructImpl<IOStreamStruct> DEBUG_IO = VariableStructImpl.valueOf("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStructImpl<OutputStreamStruct> ERROR_OUTPUT = VariableStructImpl.valueOf("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStructImpl<IOStreamStruct> QUERY_IO = VariableStructImpl.valueOf("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStructImpl<InputStreamStruct> STANDARD_INPUT = VariableStructImpl.valueOf("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStructImpl<OutputStreamStruct> STANDARD_OUTPUT = VariableStructImpl.valueOf("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
	VariableStructImpl<OutputStreamStruct> TRACE_OUTPUT = VariableStructImpl.valueOf("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, SynonymStreamStructImpl.valueOf(TERMINAL_IO));
}
