/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.functions.FuncallFunction;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.VariableStruct;

public interface CompilerVariables {

	VariableStruct<FunctionStruct> MACROEXPAND_HOOK = new VariableStruct<>("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, FuncallFunction.INSTANCE);

	VariableStruct<?> DEBUGGER_HOOK = new VariableStruct<>("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> BREAK_ON_SIGNALS = new VariableStruct<>("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<IntegerStruct> GENSYM_COUNTER = new VariableStruct<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));

	VariableStruct<ListStruct> FEATURES = new ProperListVariable("*FEATURES*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<LispStruct> COMPILE_FILE_PATHNAME = new VariableStruct<>("*COMPILE-FILE-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NullStruct.INSTANCE);
	VariableStruct<LispStruct> COMPILE_FILE_TRUENAME = new VariableStruct<>("*COMPILE-FILE-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NullStruct.INSTANCE);
	VariableStruct<LispStruct> LOAD_PATHNAME = new VariableStruct<>("*LOAD-PATHNAME*", GlobalPackageStruct.COMMON_LISP, NullStruct.INSTANCE);
	VariableStruct<LispStruct> LOAD_TRUENAME = new VariableStruct<>("*LOAD-TRUENAME*", GlobalPackageStruct.COMMON_LISP, NullStruct.INSTANCE);
	VariableStruct<BooleanStruct> COMPILE_PRINT = new VariableStruct<>("*COMPILE-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> COMPILE_VERBOSE = new VariableStruct<>("*COMPILE-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> LOAD_PRINT = new VariableStruct<>("*LOAD-PRINT*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> LOAD_VERBOSE = new VariableStruct<>("*LOAD-VERBOSE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<?> MODULES = new VariableStruct<>("*MODULES*", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<?> DASH = new VariableStruct<>("-", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> PLUS = new VariableStruct<>("+", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> PLUS_PLUS = new VariableStruct<>("++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> PLUS_PLUS_PLUS = new VariableStruct<>("+++", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> STAR = new VariableStruct<>("*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> STAR_STAR = new VariableStruct<>("**", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> STAR_STAR_STAR = new VariableStruct<>("***", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> SLASH = new VariableStruct<>("/", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> SLASH_SLASH = new VariableStruct<>("//", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<?> SLASH_SLASH_SLASH = new VariableStruct<>("///", GlobalPackageStruct.COMMON_LISP, null);

	VariableStruct<BooleanStruct> COMPILE_TOP_LEVEL = new VariableStruct<>("*COMPILE-TOP-LEVEL*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> CONVERTING_FOR_INTERPRETER = new VariableStruct<>("*CONVERTING-FOR-INTERPRETER*", GlobalPackageStruct.SYSTEM, NILStruct.INSTANCE);
}
