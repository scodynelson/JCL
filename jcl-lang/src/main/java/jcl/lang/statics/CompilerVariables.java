/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.VariableStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.NILStruct;

public interface CompilerVariables {

	VariableStruct<FunctionStruct> MACROEXPAND_HOOK = VariableStruct.valueOf("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<?> DEBUGGER_HOOK = VariableStruct.valueOf("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> BREAK_ON_SIGNALS = VariableStruct.valueOf("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<IntegerStruct> GENSYM_COUNTER = VariableStruct.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ONE);

	VariableStruct<ListStruct> FEATURES = new ProperListVariable("*FEATURES*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<LispStruct> COMPILE_FILE_PATHNAME = VariableStruct.valueOf("*COMPILE-FILE-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<LispStruct> COMPILE_FILE_TRUENAME = VariableStruct.valueOf("*COMPILE-FILE-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<LispStruct> LOAD_PATHNAME = VariableStruct.valueOf("*LOAD-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<LispStruct> LOAD_TRUENAME = VariableStruct.valueOf("*LOAD-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> COMPILE_PRINT = VariableStruct.valueOf("*COMPILE-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> COMPILE_VERBOSE = VariableStruct.valueOf("*COMPILE-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> LOAD_PRINT = VariableStruct.valueOf("*LOAD-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> LOAD_VERBOSE = VariableStruct.valueOf("*LOAD-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<?> MODULES = VariableStruct.valueOf("*MODULES*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<BooleanStruct> COMPILE_TOP_LEVEL = VariableStruct.valueOf("*COMPILE-TOP-LEVEL*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> CONVERTING_FOR_INTERPRETER = VariableStruct.valueOf("*CONVERTING-FOR-INTERPRETER*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
}
