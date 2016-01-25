/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface StreamVariables {

	VariableStruct<TwoWayStreamStruct> TERMINAL_IO = new VariableStruct<>("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP,
			new TwoWayStreamStruct(true, EmptyStreamStruct.INSTANCE, EmptyStreamStruct.INSTANCE));

	VariableStruct<IOStream> DEBUG_IO = new VariableStruct<>("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> ERROR_OUTPUT = new VariableStruct<>("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<IOStream> QUERY_IO = new VariableStruct<>("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<InputStream> STANDARD_INPUT = new VariableStruct<>("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> STANDARD_OUTPUT = new VariableStruct<>("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
	VariableStruct<OutputStream> TRACE_OUTPUT = new VariableStruct<>("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, new SynonymStreamStruct(TERMINAL_IO));
}
