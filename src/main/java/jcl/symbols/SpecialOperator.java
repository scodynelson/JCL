package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;

public final class SpecialOperator extends SymbolStruct<SpecialOperator> {

	public static final SpecialOperator BLOCK = new SpecialOperator("BLOCK", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator CATCH = new SpecialOperator("CATCH", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator EVAL_WHEN = new SpecialOperator("EVAL_WHEN", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator FLET = new SpecialOperator("FLET", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator FUNCTION = new SpecialOperator("FUNCTION", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator GO = new SpecialOperator("GO", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator IF = new SpecialOperator("IF", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LABELS = new SpecialOperator("LABELS", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LET = new SpecialOperator("LET", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LET_STAR = new SpecialOperator("LET_STAR", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LOAD_TIME_VALUE = new SpecialOperator("LOAD_TIME_VALUE", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LOCALLY = new SpecialOperator("LOCALLY", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator MACROLET = new SpecialOperator("MACROLET", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator MULTIPLE_VALUE_CALL = new SpecialOperator("MULTIPLE_VALUE_CALL", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator MULTIPLE_VALUE_PROG1 = new SpecialOperator("MULTIPLE_VALUE_PROG1", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator PROGN = new SpecialOperator("PROGN", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator PROGV = new SpecialOperator("PROGV", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator QUOTE = new SpecialOperator("QUOTE", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator RETURN_FROM = new SpecialOperator("RETURN_FROM", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator SETQ = new SpecialOperator("SETQ", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator SYMBOL_MACROLET = new SpecialOperator("SYMBOL_MACROLET", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator TAGBODY = new SpecialOperator("TAGBODY", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator THE = new SpecialOperator("THE", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator THROW = new SpecialOperator("THROW", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator UNWIND_PROTECT = new SpecialOperator("UNWIND_PROTECT", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperator DECLARE = new SpecialOperator("DECLARE", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator LAMBDA = new SpecialOperator("LAMBDA", GlobalPackageStruct.COMMON_LISP);
	public static final SpecialOperator MACRO_LAMBDA = new SpecialOperator("MACRO-LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperator DEFSTRUCT = new SpecialOperator("%DEFSTRUCT", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator FUNCTION_MARKER = new SpecialOperator("%FUNCTION-MARKER", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator LAMBDA_MARKER = new SpecialOperator("%LAMBDA", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator FLET_MARKER = new SpecialOperator("%FLET", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator LABELS_MARKER = new SpecialOperator("%LABELS", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator LET_MARKER = new SpecialOperator("%LET", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator MACRO_MARKER = new SpecialOperator("%MACRO", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator STATIC_FIELD = new SpecialOperator("%STATIC-FIELD", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator TAIL_CALL = new SpecialOperator("%TAIL-CALL", GlobalPackageStruct.COMPILER);
	public static final SpecialOperator TAIL_RECURSION = new SpecialOperator("%TAIL-RECURSION", GlobalPackageStruct.COMPILER);

	private SpecialOperator(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}
}
