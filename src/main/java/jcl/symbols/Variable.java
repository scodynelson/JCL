package jcl.symbols;

import jcl.LispStruct;
import jcl.compiler.old.functions.BaseMacroExpandFn;
import jcl.functions.FunctionStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RandomStateStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.readtables.ReadtableStruct;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Variable<T extends LispStruct> extends SymbolStruct<T> {

	public static final Variable<FunctionStruct> MACROEXPAND_HOOK = new Variable<>("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, BaseMacroExpandFn.FUNCTION);

	public static final Variable DEBUGGER_HOOK = new Variable("*DEBUGGER-HOOK*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable BREAK_ON_SIGNALS = new Variable("*BREAK-ON-SIGNALS*", GlobalPackageStruct.COMMON_LISP, null);

	public static final Variable<IntegerStruct> GENSYM_COUNTER = new Variable<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));

	public static final Variable<PackageStruct> PACKAGE = new Variable<>("*PACKAGE*", GlobalPackageStruct.COMMON_LISP, GlobalPackageStruct.COMMON_LISP_USER);

	public static final Variable<RandomStateStruct> RANDOM_STATE = new Variable<>("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, new RandomStateStruct());

	public static final Variable DEFAULT_PATHNAME_DEFAULTS = new Variable("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, null);

	public static final Variable DEBUG_IO = new Variable("*DEBUG-IO*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable ERROR_OUTPUT = new Variable("*ERROR-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable QUERY_IO = new Variable("*QUERY-IO*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable STANDARD_INPUT = new Variable("*STANDARD-INPUT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable STANDARD_OUTPUT = new Variable("*STANDARD-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable TRACE_OUTPUT = new Variable("*TRACE-OUTPUT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable TERMINAL_IO = new Variable("*TERMINAL-IO*", GlobalPackageStruct.COMMON_LISP, null);

	public static final Variable<BooleanStruct<?>> PRINT_ARRAY = new Variable<>("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	public static final Variable<IntegerStruct> PRINT_BASE = new Variable<>("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.TEN));
	public static final Variable<BooleanStruct<?>> PRINT_RADIX = new Variable<>("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	public static final Variable PRINT_CASE = new Variable("*PRINT-CASE*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<BooleanStruct<?>> PRINT_CIRCLE = new Variable<>("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	public static final Variable<BooleanStruct<?>> PRINT_ESCAPE = new Variable<>("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	public static final Variable<BooleanStruct<?>> PRINT_GENSYM = new Variable<>("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	public static final Variable<IntegerStruct> PRINT_LEVEL = new Variable<>("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<IntegerStruct> PRINT_LENGTH = new Variable<>("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<IntegerStruct> PRINT_LINES = new Variable<>("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<IntegerStruct> PRINT_MISER_WIDTH = new Variable<>("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable PRINT_PPRINT_DISPATCH = new Variable("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<BooleanStruct<?>> PRINT_PRETTY = new Variable<>("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	public static final Variable<BooleanStruct<?>> PRINT_READABLY = new Variable<>("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	public static final Variable<IntegerStruct> PRINT_RIGHT_MARGIN = new Variable<>("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP, null);

	public static final Variable READ_BASE = new Variable("*READ-BASE*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable READ_DEFAULT_FLOAT_FORMAT = new Variable("*READ-DEFAULT-FLOAT-FORMAT*", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.ZERO)); // TODO: fix this!!!
	public static final Variable READ_EVAL = new Variable("*READ-EVAL*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable READ_SUPPRESS = new Variable("*READ-SUPPRESS*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable<ReadtableStruct> READTABLE = new Variable<>("*READTABLE*", GlobalPackageStruct.COMMON_LISP, new ReadtableStruct());

	public static final Variable FEATURES = new Variable("*FEATURES*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable COMPILE_FILE_PATHNAME = new Variable("*COMPILE-FILE-PATHNAME*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable COMPILE_FILE_TRUENAME = new Variable("*COMPILE-FILE-TRUENAME*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable LOAD_PATHNAME = new Variable("*LOAD-PATHNAME*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable LOAD_TRUENAME = new Variable("*LOAD-TRUENAME*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable COMPILE_PRINT = new Variable("*COMPILE-PRINT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable COMPILE_VERBOSE = new Variable("*COMPILE-VERBOSE*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable LOAD_PRINT = new Variable("*LOAD-PRINT*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable LOAD_VERBOSE = new Variable("*LOAD-VERBOSE*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable MODULES = new Variable("*MODULES*", GlobalPackageStruct.COMMON_LISP, null);

	public static final Variable DASH = new Variable("-", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable PLUS = new Variable("+", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable PLUS_PLUS = new Variable("++", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable PLUS_PLUS_PLUS = new Variable("+++", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable STAR = new Variable("*", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable STAR_STAR = new Variable("**", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable STAR_STAR_STAR = new Variable("***", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable SLASH = new Variable("/", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable SLASH_SLASH = new Variable("//", GlobalPackageStruct.COMMON_LISP, null);
	public static final Variable SLASH_SLASH_SLASH = new Variable("///", GlobalPackageStruct.COMMON_LISP, null);

	private Variable(final String name, final PackageStruct symbolPackage, final T value) {
		super(name, symbolPackage, value);
	}
}
