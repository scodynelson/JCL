package jcl.symbols;

import jcl.LispStruct;
import jcl.compiler.old.functions.BaseMacroExpandFn;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RandomStateStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;

import java.math.BigInteger;

public final class Variable<T extends LispStruct> extends SymbolStruct<T> {

	public static final Variable<FunctionStruct> MACROEXPAND_HOOK = new Variable<>("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, BaseMacroExpandFn.FUNCTION);

	public static final Variable DEBUGGER_HOOK = new Variable("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable BREAK_ON_SIGNALS = new Variable("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable<IntegerStruct> GENSYM_COUNTER = new Variable<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));

	public static final Variable<PackageStruct> PACKAGE = new Variable<>("*PACKAGE*", GlobalPackageStruct.COMMON_LISP, GlobalPackageStruct.COMMON_LISP_USER);

	public static final Variable<RandomStateStruct> RANDOM_STATE = new Variable<>("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, new RandomStateStruct());

	public static final Variable DEFAULT_PATHNAME_DEFAULTS = new Variable("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable DEBUG_IO = new Variable("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable ERROR_OUTPUT = new Variable("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable QUERY_IO = new Variable("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable STANDARD_INPUT = new Variable("*STANDARD_INPUT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable STANDARD_OUTPUT = new Variable("*STANDARD_OUTPUT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable TRACE_OUTPUT = new Variable("*TRACE_OUTPUT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable TERMINAL_IO = new Variable("*TERMINAL_IO*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable PRINT_ARRAY = new Variable("*PRINT_ARRAY*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_BASE = new Variable("*PRINT_BASE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_RADIX = new Variable("*PRINT_RADIX*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_CASE = new Variable("*PRINT_CASE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_CIRCLE = new Variable("*PRINT_CIRCLE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_ESCAPE = new Variable("*PRINT_ESCAPE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_GENSYM = new Variable("*PRINT_GENSYM*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_LEVEL = new Variable("*PRINT_LEVEL*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_LENGTH = new Variable("*PRINT_LENGTH*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_LINES = new Variable("*PRINT_LINES*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_MISER_WIDTH = new Variable("*PRINT_MISER_WIDTH*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_PPRINT_DISPATCH = new Variable("*PRINT_PPRINT_DISPATCH*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_PRETTY = new Variable("*PRINT_PRETTY*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_READABLY = new Variable("*PRINT_READABLY*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PRINT_RIGHT_MARGIN = new Variable("*PRINT_RIGHT_MARGIN*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable READ_BASE = new Variable("*READ_BASE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable READ_DEFAULT_FLOAT_FORMAT = new Variable("*READ_DEFAULT_FLOAT_FORMAT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable READ_EVAL = new Variable("*READ_EVAL*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable READ_SUPPRESS = new Variable("*READ_SUPPRESS*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable READTABLE = new Variable("*READTABLE*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable FEATURES = new Variable("*FEATURES*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable COMPILE_FILE_PATHNAME = new Variable("*COMPILE_FILE_PATHNAME*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable COMPILE_FILE_TRUENAME = new Variable("*COMPILE_FILE_TRUENAME*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable LOAD_PATHNAME = new Variable("*LOAD_PATHNAME*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable LOAD_TRUENAME = new Variable("*LOAD_TRUENAME*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable COMPILE_PRINT = new Variable("*COMPILE_PRINT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable COMPILE_VERBOSE = new Variable("*COMPILE_VERBOSE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable LOAD_PRINT = new Variable("*LOAD_PRINT*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable LOAD_VERBOSE = new Variable("*LOAD_VERBOSE*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable MODULES = new Variable("*MODULES*", GlobalPackageStruct.COMMON_LISP);

	public static final Variable DASH = new Variable("-", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PLUS = new Variable("+", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PLUS_PLUS = new Variable("++", GlobalPackageStruct.COMMON_LISP);
	public static final Variable PLUS_PLUS_PLUS = new Variable("+++", GlobalPackageStruct.COMMON_LISP);
	public static final Variable STAR = new Variable("*", GlobalPackageStruct.COMMON_LISP);
	public static final Variable STAR_STAR = new Variable("**", GlobalPackageStruct.COMMON_LISP);
	public static final Variable STAR_STAR_STAR = new Variable("***", GlobalPackageStruct.COMMON_LISP);
	public static final Variable SLASH = new Variable("/", GlobalPackageStruct.COMMON_LISP);
	public static final Variable SLASH_SLASH = new Variable("//", GlobalPackageStruct.COMMON_LISP);
	public static final Variable SLASH_SLASH_SLASH = new Variable("///", GlobalPackageStruct.COMMON_LISP);

	private Variable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}

	private Variable(final String name, final PackageStruct symbolPackage, final T value) {
		super(name, symbolPackage, value);
	}
}
