/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real;

import jcl.compiler.old.functions.BaseMacroExpandFn;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Variable;

import java.math.BigInteger;

public interface CompilerVariables {

    Variable<FunctionStruct> MACROEXPAND_HOOK = new Variable<>("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, BaseMacroExpandFn.FUNCTION);

    Variable<?> DEBUGGER_HOOK = new Variable<>("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> BREAK_ON_SIGNALS = new Variable<>("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP, null);

    Variable<IntegerStruct> GENSYM_COUNTER = new Variable<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));

    Variable<ListStruct> FEATURES = new ProperListVariable("*FEATURES*", GlobalPackageStruct.COMMON_LISP);
    Variable<?> COMPILE_FILE_PATHNAME = new Variable<>("*COMPILE-FILE-PATHNAME*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> COMPILE_FILE_TRUENAME = new Variable<>("*COMPILE-FILE-TRUENAME*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> LOAD_PATHNAME = new Variable<>("*LOAD-PATHNAME*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> LOAD_TRUENAME = new Variable<>("*LOAD-TRUENAME*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> COMPILE_PRINT = new Variable<>("*COMPILE-PRINT*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> COMPILE_VERBOSE = new Variable<>("*COMPILE-VERBOSE*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> LOAD_PRINT = new Variable<>("*LOAD-PRINT*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> LOAD_VERBOSE = new Variable<>("*LOAD-VERBOSE*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> MODULES = new Variable<>("*MODULES*", GlobalPackageStruct.COMMON_LISP, null);

    Variable<?> DASH = new Variable<>("-", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> PLUS = new Variable<>("+", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> PLUS_PLUS = new Variable<>("++", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> PLUS_PLUS_PLUS = new Variable<>("+++", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> STAR = new Variable<>("*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> STAR_STAR = new Variable<>("**", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> STAR_STAR_STAR = new Variable<>("***", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> SLASH = new Variable<>("/", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> SLASH_SLASH = new Variable<>("//", GlobalPackageStruct.COMMON_LISP, null);
    Variable<?> SLASH_SLASH_SLASH = new Variable<>("///", GlobalPackageStruct.COMMON_LISP, null);
}
