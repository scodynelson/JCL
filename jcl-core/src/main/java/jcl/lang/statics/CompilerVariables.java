/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.NILStruct;

public interface CompilerVariables {

	VariableStructImpl<FunctionStruct> MACROEXPAND_HOOK = VariableStructImpl.valueOf("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStructImpl<?> DEBUGGER_HOOK = VariableStructImpl.valueOf("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<?> BREAK_ON_SIGNALS = VariableStructImpl.valueOf("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStructImpl<IntegerStruct> GENSYM_COUNTER = VariableStructImpl.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ONE);

	VariableStructImpl<ListStruct> FEATURES = new ProperListVariable("*FEATURES*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<LispStruct> COMPILE_FILE_PATHNAME = VariableStructImpl.valueOf("*COMPILE-FILE-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<LispStruct> COMPILE_FILE_TRUENAME = VariableStructImpl.valueOf("*COMPILE-FILE-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<LispStruct> LOAD_PATHNAME = VariableStructImpl.valueOf("*LOAD-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<LispStruct> LOAD_TRUENAME = VariableStructImpl.valueOf("*LOAD-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> COMPILE_PRINT = VariableStructImpl.valueOf("*COMPILE-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> COMPILE_VERBOSE = VariableStructImpl.valueOf("*COMPILE-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> LOAD_PRINT = VariableStructImpl.valueOf("*LOAD-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> LOAD_VERBOSE = VariableStructImpl.valueOf("*LOAD-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<?> MODULES = VariableStructImpl.valueOf("*MODULES*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStructImpl<BooleanStruct> COMPILE_TOP_LEVEL = VariableStructImpl.valueOf("*COMPILE-TOP-LEVEL*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> CONVERTING_FOR_INTERPRETER = VariableStructImpl.valueOf("*CONVERTING-FOR-INTERPRETER*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
}
