package jcl.symbols;

import jcl.LispStruct;
import jcl.numbers.IntegerStruct;

public final class Variable<T extends LispStruct> extends SymbolStruct<T> {

	public static final Variable MACROEXPAND_HOOK = new Variable("*MACROEXPAND-HOOK*");

	public static final Variable DEBUGGER_HOOK = new Variable("*DEBUGGER-HOOK*");
	public static final Variable BREAK_ON_SIGNALS = new Variable("*BREAK-ON-SIGNALS*");

	public static final Variable<IntegerStruct> GENSYM_COUNTER = new Variable<>("*GENSYM-COUNTER*");

	public static final Variable PACKAGE = new Variable("*PACKAGE*");

	public static final Variable RANDOM_STATE = new Variable("*RANDOM-STATE*");

	public static final Variable DEFAULT_PATHNAME_DEFAULTS = new Variable("*DEFAULT-PATHNAME-DEFAULTS*");

	public static final Variable DEBUG_IO = new Variable("*DEBUG-IO*");
	public static final Variable ERROR_OUTPUT = new Variable("*ERROR-OUTPUT*");
	public static final Variable QUERY_IO = new Variable("*QUERY-IO*");
	public static final Variable STANDARD_INPUT = new Variable("*STANDARD_INPUT*");
	public static final Variable STANDARD_OUTPUT = new Variable("*STANDARD_OUTPUT*");
	public static final Variable TRACE_OUTPUT = new Variable("*TRACE_OUTPUT*");
	public static final Variable TERMINAL_IO = new Variable("*TERMINAL_IO*");

	public static final Variable PRINT_ARRAY = new Variable("*PRINT_ARRAY*");
	public static final Variable PRINT_BASE = new Variable("*PRINT_BASE*");
	public static final Variable PRINT_RADIX = new Variable("*PRINT_RADIX*");
	public static final Variable PRINT_CASE = new Variable("*PRINT_CASE*");
	public static final Variable PRINT_CIRCLE = new Variable("*PRINT_CIRCLE*");
	public static final Variable PRINT_ESCAPE = new Variable("*PRINT_ESCAPE*");
	public static final Variable PRINT_GENSYM = new Variable("*PRINT_GENSYM*");
	public static final Variable PRINT_LEVEL = new Variable("*PRINT_LEVEL*");
	public static final Variable PRINT_LENGTH = new Variable("*PRINT_LENGTH*");
	public static final Variable PRINT_LINES = new Variable("*PRINT_LINES*");
	public static final Variable PRINT_MISER_WIDTH = new Variable("*PRINT_MISER_WIDTH*");
	public static final Variable PRINT_PPRINT_DISPATCH = new Variable("*PRINT_PPRINT_DISPATCH*");
	public static final Variable PRINT_PRETTY = new Variable("*PRINT_PRETTY*");
	public static final Variable PRINT_READABLY = new Variable("*PRINT_READABLY*");
	public static final Variable PRINT_RIGHT_MARGIN = new Variable("*PRINT_RIGHT_MARGIN*");

	public static final Variable READ_BASE = new Variable("*READ_BASE*");
	public static final Variable READ_DEFAULT_FLOAT_FORMAT = new Variable("*READ_DEFAULT_FLOAT_FORMAT*");
	public static final Variable READ_EVAL = new Variable("*READ_EVAL*");
	public static final Variable READ_SUPPRESS = new Variable("*READ_SUPPRESS*");
	public static final Variable READTABLE = new Variable("*READTABLE*");

	public static final Variable FEATURES = new Variable("*FEATURES*");
	public static final Variable COMPILE_FILE_PATHNAME = new Variable("*COMPILE_FILE_PATHNAME*");
	public static final Variable COMPILE_FILE_TRUENAME = new Variable("*COMPILE_FILE_TRUENAME*");
	public static final Variable LOAD_PATHNAME = new Variable("*LOAD_PATHNAME*");
	public static final Variable LOAD_TRUENAME = new Variable("*LOAD_TRUENAME*");
	public static final Variable COMPILE_PRINT = new Variable("*COMPILE_PRINT*");
	public static final Variable COMPILE_VERBOSE = new Variable("*COMPILE_VERBOSE*");
	public static final Variable LOAD_PRINT = new Variable("*LOAD_PRINT*");
	public static final Variable LOAD_VERBOSE = new Variable("*LOAD_VERBOSE*");
	public static final Variable MODULES = new Variable("*MODULES*");

	public static final Variable DASH = new Variable("-");
	public static final Variable PLUS = new Variable("+");
	public static final Variable PLUS_PLUS = new Variable("++");
	public static final Variable PLUS_PLUS_PLUS = new Variable("+++");
	public static final Variable STAR = new Variable("*");
	public static final Variable STAR_STAR = new Variable("**");
	public static final Variable STAR_STAR_STAR = new Variable("***");
	public static final Variable SLASH = new Variable("/");
	public static final Variable SLASH_SLASH = new Variable("//");
	public static final Variable SLASH_SLASH_SLASH = new Variable("///");

	private Variable(final String name) {
		super(name);
	}
}
