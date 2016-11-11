package jcl.lang.internal;

import jcl.lang.PackageStruct;
import jcl.lang.statics.GlobalPackageStruct;

public final class SpecialOperatorStructImpl extends SymbolStructImpl {

	public static final SpecialOperatorStructImpl BLOCK = new SpecialOperatorStructImpl("BLOCK", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl CATCH = new SpecialOperatorStructImpl("CATCH", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl EVAL_WHEN = new SpecialOperatorStructImpl("EVAL-WHEN", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl FLET = new SpecialOperatorStructImpl("FLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl FUNCTION = new SpecialOperatorStructImpl("FUNCTION", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl GO = new SpecialOperatorStructImpl("GO", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl IF = new SpecialOperatorStructImpl("IF", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LABELS = new SpecialOperatorStructImpl("LABELS", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LET = new SpecialOperatorStructImpl("LET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LET_STAR = new SpecialOperatorStructImpl("LET*", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LOAD_TIME_VALUE = new SpecialOperatorStructImpl("LOAD-TIME-VALUE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LOCALLY = new SpecialOperatorStructImpl("LOCALLY", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl MACROLET = new SpecialOperatorStructImpl("MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl MULTIPLE_VALUE_CALL = new SpecialOperatorStructImpl("MULTIPLE-VALUE-CALL", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl MULTIPLE_VALUE_PROG1 = new SpecialOperatorStructImpl("MULTIPLE-VALUE-PROG1", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl PROGN = new SpecialOperatorStructImpl("PROGN", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl PROGV = new SpecialOperatorStructImpl("PROGV", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl QUOTE = new SpecialOperatorStructImpl("QUOTE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl RETURN_FROM = new SpecialOperatorStructImpl("RETURN-FROM", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl SETQ = new SpecialOperatorStructImpl("SETQ", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl SYMBOL_MACROLET = new SpecialOperatorStructImpl("SYMBOL-MACROLET", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl TAGBODY = new SpecialOperatorStructImpl("TAGBODY", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl THE = new SpecialOperatorStructImpl("THE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl THROW = new SpecialOperatorStructImpl("THROW", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl UNWIND_PROTECT = new SpecialOperatorStructImpl("UNWIND-PROTECT", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl DECLARE = new SpecialOperatorStructImpl("DECLARE", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl LAMBDA = new SpecialOperatorStructImpl("LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl DEFSTRUCT = new SpecialOperatorStructImpl("%DEFSTRUCT", GlobalPackageStruct.COMPILER);

	// TODO: Temporarily having this in the COMMON_LISP package
	public static final SpecialOperatorStructImpl MACRO_LAMBDA = new SpecialOperatorStructImpl("MACRO-LAMBDA", GlobalPackageStruct.COMMON_LISP);

	public static final SpecialOperatorStructImpl TAIL_CALL = new SpecialOperatorStructImpl("%TAIL-CALL", GlobalPackageStruct.COMPILER);

	public static final SpecialOperatorStructImpl TAIL_RECURSION = new SpecialOperatorStructImpl("%TAIL-RECURSION", GlobalPackageStruct.COMPILER);

	private SpecialOperatorStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}
}
