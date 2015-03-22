package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;

public final class SpecialOperatorStruct extends SymbolStruct<SpecialOperatorStruct> {

	public static final SpecialOperatorStruct BLOCK = new SpecialOperatorStruct("BLOCK", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct CATCH = new SpecialOperatorStruct("CATCH", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct EVAL_WHEN = new SpecialOperatorStruct("EVAL-WHEN", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct FLET = new SpecialOperatorStruct("FLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct FUNCTION = new SpecialOperatorStruct("FUNCTION", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct GO = new SpecialOperatorStruct("GO", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct IF = new SpecialOperatorStruct("IF", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LABELS = new SpecialOperatorStruct("LABELS", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LET = new SpecialOperatorStruct("LET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LET_STAR = new SpecialOperatorStruct("LET*", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LOAD_TIME_VALUE = new SpecialOperatorStruct("LOAD-TIME-VALUE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LOCALLY = new SpecialOperatorStruct("LOCALLY", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct MACROLET = new SpecialOperatorStruct("MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct MULTIPLE_VALUE_CALL = new SpecialOperatorStruct("MULTIPLE-VALUE-CALL", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct MULTIPLE_VALUE_PROG1 = new SpecialOperatorStruct("MULTIPLE-VALUE-PROG1", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct PROGN = new SpecialOperatorStruct("PROGN", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct PROGV = new SpecialOperatorStruct("PROGV", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct QUOTE = new SpecialOperatorStruct("QUOTE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct RETURN_FROM = new SpecialOperatorStruct("RETURN-FROM", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct SETQ = new SpecialOperatorStruct("SETQ", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct SYMBOL_MACROLET = new SpecialOperatorStruct("SYMBOL-MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct TAGBODY = new SpecialOperatorStruct("TAGBODY", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct THE = new SpecialOperatorStruct("THE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct THROW = new SpecialOperatorStruct("THROW", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct UNWIND_PROTECT = new SpecialOperatorStruct("UNWIND-PROTECT", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct DECLARE = new SpecialOperatorStruct("DECLARE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct LAMBDA = new SpecialOperatorStruct("LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStruct DEFSTRUCT = new SpecialOperatorStruct("%DEFSTRUCT", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct FUNCTION_MARKER = new SpecialOperatorStruct("%FUNCTION-MARKER", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct LAMBDA_MARKER = new SpecialOperatorStruct("%LAMBDA", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct FLET_MARKER = new SpecialOperatorStruct("%FLET", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct LABELS_MARKER = new SpecialOperatorStruct("%LABELS", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct LET_MARKER = new SpecialOperatorStruct("%LET", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct MACRO_MARKER = new SpecialOperatorStruct("%MACRO", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct STATIC_FIELD = new SpecialOperatorStruct("%STATIC-FIELD", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct TAIL_CALL = new SpecialOperatorStruct("%TAIL-CALL", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStruct TAIL_RECURSION = new SpecialOperatorStruct("%TAIL-RECURSION", GlobalPackageStruct.COMPILER);

	private static final long serialVersionUID = 6627556964283935128L;

	private SpecialOperatorStruct(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}
}
