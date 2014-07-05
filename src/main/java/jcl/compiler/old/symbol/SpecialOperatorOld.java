package jcl.compiler.old.symbol;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

public class SpecialOperatorOld {

	public static final SymbolStruct BLOCK = new SymbolStruct("BLOCK", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct CATCH = new SymbolStruct("CATCH", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct DECLARE = new SymbolStruct("DECLARE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct EVAL_WHEN = new SymbolStruct("EVAL-WHEN", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct FLET = new SymbolStruct("FLET", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct FUNCTION = new SymbolStruct("FUNCTION", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct GO = new SymbolStruct("GO", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct IF = new SymbolStruct("IF", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LABELS = new SymbolStruct("LABELS", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LAMBDA = new SymbolStruct("LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LET = new SymbolStruct("LET", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LET_STAR = new SymbolStruct("LET*", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LOAD_TIME_VALUE = new SymbolStruct("LOAD-TIME-VALUE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct LOCALLY = new SymbolStruct("LOCALLY", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct MACROLET = new SymbolStruct("MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct MACRO_LAMBDA = new SymbolStruct("MACRO-LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct MULTIPLE_VALUE_CALL = new SymbolStruct("MULTIPLE-VALUE-CALL", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct MULTIPLE_VALUE_PROG1 = new SymbolStruct("MULTIPLE-VALUE-PROG1", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct PROGN = new SymbolStruct("PROGN", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct PROGV = new SymbolStruct("PROGV", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct QUOTE = new SymbolStruct("QUOTE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct RETURN_FROM = new SymbolStruct("RETURN-FROM", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SETQ = new SymbolStruct("SETQ", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct SYMBOL_MACROLET = new SymbolStruct("SYMBOL-MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct TAGBODY = new SymbolStruct("TAGBODY", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct THE = new SymbolStruct("THE", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct THROW = new SymbolStruct("THROW", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct UNWIND_PROTECT = new SymbolStruct("UNWIND-PROTECT", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct DEFSTRUCT = new SymbolStruct("%DEFSTRUCT", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct FUNCTION_MARKER = new SymbolStruct("%FUNCTION-MARKER%", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct LAMBDA_MARKER = new SymbolStruct("%LAMBDA", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct FLET_MARKER = new SymbolStruct("%FLET", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct LABELS_MARKER = new SymbolStruct("%LABELS", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct LET_MARKER = new SymbolStruct("%LET", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct MACRO_MARKER = new SymbolStruct("%MACRO", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct STATIC_FIELD = new SymbolStruct("%STATIC-FIELD", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct TAIL_CALL = new SymbolStruct("%TAIL-CALL", GlobalPackageStruct.COMPILER);

	public static final SymbolStruct TAIL_RECURSION = new SymbolStruct("%TAIL-RECURSION", GlobalPackageStruct.COMPILER);
}
