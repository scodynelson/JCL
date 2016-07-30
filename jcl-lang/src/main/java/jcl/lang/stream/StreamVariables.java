/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.VariableStruct;

public interface StreamVariables {

	VariableStruct<TwoWayStreamStruct> TERMINAL_IO = VariableStruct.valueOf("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP,
	                                                                      new TwoWayStreamStruct(true, EmptyStreamStruct.INSTANCE, EmptyStreamStruct.INSTANCE));

	VariableStruct<IOStream> DEBUG_IO = VariableStruct.valueOf("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> ERROR_OUTPUT = VariableStruct.valueOf("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<IOStream> QUERY_IO = VariableStruct.valueOf("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<InputStream> STANDARD_INPUT = VariableStruct.valueOf("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> STANDARD_OUTPUT = VariableStruct.valueOf("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> TRACE_OUTPUT = VariableStruct.valueOf("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
}
