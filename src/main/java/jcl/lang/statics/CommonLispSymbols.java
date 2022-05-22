package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.FloatStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.internal.ConstantStructImpl;
import jcl.lang.internal.VariableStructImpl;

/**
 * The {@link CommonLispSymbols} is the global location for system defined symbols.
 */
@SuppressWarnings("all")
public interface CommonLispSymbols {

	ConstantStructImpl<?> AND_ALLOW_OTHER_KEYS = internAndExportCLConstant("&ALLOW-OTHER-KEYS");
	ConstantStructImpl<?> AND_AUX = internAndExportCLConstant("&AUX");
	ConstantStructImpl<?> AND_BODY = internAndExportCLConstant("&BODY");
	ConstantStructImpl<?> AND_ENVIRONMENT = internAndExportCLConstant("&ENVIRONMENT");
	ConstantStructImpl<?> AND_KEY = internAndExportCLConstant("&KEY");
	ConstantStructImpl<?> AND_OPTIONAL = internAndExportCLConstant("&OPTIONAL");
	ConstantStructImpl<?> AND_REST = internAndExportCLConstant("&REST");
	ConstantStructImpl<?> AND_WHOLE = internAndExportCLConstant("&WHOLE");

	ConstantStructImpl<IntegerStruct> ARRAY_DIMENSION_LIMIT = internAndExportCLConstant("ARRAY-DIMENSION-LIMIT");
	ConstantStructImpl<IntegerStruct> ARRAY_RANK_LIMIT = internAndExportCLConstant("ARRAY-RANK-LIMIT");
	ConstantStructImpl<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = internAndExportCLConstant("ARRAY-TOTAL-SIZE-LIMIT");

	ConstantStructImpl<IntegerStruct> BOOLE_1 = internAndExportCLConstant("BOOLE-1");
	ConstantStructImpl<IntegerStruct> BOOLE_2 = internAndExportCLConstant("BOOLE-2");
	ConstantStructImpl<IntegerStruct> BOOLE_AND = internAndExportCLConstant("BOOLE-AND");
	ConstantStructImpl<IntegerStruct> BOOLE_ANDC1 = internAndExportCLConstant("BOOLE-ANDC1");
	ConstantStructImpl<IntegerStruct> BOOLE_ANDC2 = internAndExportCLConstant("BOOLE-ANDC2");
	ConstantStructImpl<IntegerStruct> BOOLE_C1 = internAndExportCLConstant("BOOLE-C1");
	ConstantStructImpl<IntegerStruct> BOOLE_C2 = internAndExportCLConstant("BOOLE-C2");
	ConstantStructImpl<IntegerStruct> BOOLE_CLR = internAndExportCLConstant("BOOLE-CLR");
	ConstantStructImpl<IntegerStruct> BOOLE_EQV = internAndExportCLConstant("BOOLE-EQV");
	ConstantStructImpl<IntegerStruct> BOOLE_IOR = internAndExportCLConstant("BOOLE-IOR");
	ConstantStructImpl<IntegerStruct> BOOLE_NAND = internAndExportCLConstant("BOOLE-NAND");
	ConstantStructImpl<IntegerStruct> BOOLE_NOR = internAndExportCLConstant("BOOLE-NOR");
	ConstantStructImpl<IntegerStruct> BOOLE_ORC1 = internAndExportCLConstant("BOOLE-ORC1");
	ConstantStructImpl<IntegerStruct> BOOLE_ORC2 = internAndExportCLConstant("BOOLE-ORC2");
	ConstantStructImpl<IntegerStruct> BOOLE_SET = internAndExportCLConstant("BOOLE-SET");
	ConstantStructImpl<IntegerStruct> BOOLE_XOR = internAndExportCLConstant("BOOLE-XOR");

	ConstantStructImpl<IntegerStruct> MOST_POSITIVE_FIXNUM = internAndExportCLConstant("MOST-POSITIVE-FIXNUM");
	ConstantStructImpl<IntegerStruct> MOST_NEGATIVE_FIXNUM = internAndExportCLConstant("MOST-NEGATIVE-FIXNUM");

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = internAndExportCLConstant("MOST-POSITIVE-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = internAndExportCLConstant("MOST-NEGATIVE-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT");
	ConstantStructImpl<FloatStruct> SHORT_FLOAT_EPSILON = internAndExportCLConstant("SHORT-FLOAT-EPSILON");
	ConstantStructImpl<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = internAndExportCLConstant("SHORT-FLOAT-NEGATIVE-EPSILON");

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = internAndExportCLConstant("MOST-POSITIVE-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = internAndExportCLConstant("MOST-NEGATIVE-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT");
	ConstantStructImpl<FloatStruct> SINGLE_FLOAT_EPSILON = internAndExportCLConstant("SINGLE-FLOAT-EPSILON");
	ConstantStructImpl<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = internAndExportCLConstant("SINGLE-FLOAT-NEGATIVE-EPSILON");

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = internAndExportCLConstant("MOST-POSITIVE-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = internAndExportCLConstant("MOST-NEGATIVE-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT");
	ConstantStructImpl<FloatStruct> DOUBLE_FLOAT_EPSILON = internAndExportCLConstant("DOUBLE-FLOAT-EPSILON");
	ConstantStructImpl<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = internAndExportCLConstant("DOUBLE-FLOAT-NEGATIVE-EPSILON");

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_LONG_FLOAT = internAndExportCLConstant("MOST-POSITIVE-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = internAndExportCLConstant("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = internAndExportCLConstant("MOST-NEGATIVE-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = internAndExportCLConstant("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT");
	ConstantStructImpl<FloatStruct> LONG_FLOAT_EPSILON = internAndExportCLConstant("LONG-FLOAT-EPSILON");
	ConstantStructImpl<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = internAndExportCLConstant("LONG-FLOAT-NEGATIVE-EPSILON");

	ConstantStructImpl<FloatStruct> PI = internAndExportCLConstant("PI");

	ConstantStructImpl<IntegerStruct> CALL_ARGUMENTS_LIMIT = internAndExportCLConstant("CALL-ARGUMENTS-LIMIT");

	ConstantStructImpl<IntegerStruct> CHAR_CODE_LIMIT = internAndExportCLConstant("CHAR-CODE-LIMIT");
	ConstantStructImpl<IntegerStruct> INTERNAL_TIME_UNITS_PER_SECOND = internAndExportCLConstant("INTERNAL-TIME-UNITS-PER-SECOND");
	ConstantStructImpl<ListStruct> LAMBDA_LIST_KEYWORDS = internAndExportCLConstant("LAMBDA-LIST-KEYWORDS");
	ConstantStructImpl<IntegerStruct> LAMBDA_PARAMETERS_LIMIT = internAndExportCLConstant("LAMBDA-PARAMETERS-LIMIT");
	ConstantStructImpl<IntegerStruct> MULTIPLE_VALUES_LIMIT = internAndExportCLConstant("MULTIPLE-VALUES-LIMIT");

	SymbolStruct T = TStruct.INSTANCE;
	SymbolStruct NIL = NILStruct.INSTANCE;

	SymbolStruct COMPILATION_SPEED = internAndExportCLSymbol("COMPILATION-SPEED");
	SymbolStruct DEBUG = internAndExportCLSymbol("DEBUG");
	SymbolStruct DECLARATION = internAndExportCLSymbol("DECLARATION");
	SymbolStruct DYNAMIC_EXTENT = internAndExportCLSymbol("DYNAMIC-EXTENT");
	SymbolStruct FTYPE = internAndExportCLSymbol("FTYPE");
	SymbolStruct IGNORABLE = internAndExportCLSymbol("IGNORABLE");
	SymbolStruct IGNORE = internAndExportCLSymbol("IGNORE");
	SymbolStruct INLINE = internAndExportCLSymbol("INLINE");
	SymbolStruct NOTINLINE = internAndExportCLSymbol("NOTINLINE");
	SymbolStruct OPTIMIZE = internAndExportCLSymbol("OPTIMIZE");
	SymbolStruct SAFETY = internAndExportCLSymbol("SAFETY");
	SymbolStruct SPACE = internAndExportCLSymbol("SPACE");
	SymbolStruct SPECIAL = internAndExportCLSymbol("SPECIAL");
	SymbolStruct SPEED = internAndExportCLSymbol("SPEED");
	SymbolStruct TYPE = internAndExportCLSymbol("TYPE");

	VariableStructImpl<?> BREAK_ON_SIGNALS_VAR = internAndExportCLVariable("*BREAK-ON-SIGNALS*");
	VariableStructImpl<LispStruct> COMPILE_FILE_PATHNAME_VAR = internAndExportCLVariable("*COMPILE-FILE-PATHNAME*");
	VariableStructImpl<LispStruct> COMPILE_FILE_TRUENAME_VAR = internAndExportCLVariable("*COMPILE-FILE-TRUENAME*");
	VariableStructImpl<BooleanStruct> COMPILE_PRINT_VAR = internAndExportCLVariable("*COMPILE-PRINT*");
	VariableStructImpl<BooleanStruct> COMPILE_VERBOSE_VAR = internAndExportCLVariable("*COMPILE-VERBOSE*");
	VariableStructImpl<?> DEBUGGER_HOOK_VAR = internAndExportCLVariable("*DEBUGGER-HOOK*");
	VariableStructImpl<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS_VAR = internAndExportCLVariable("*DEFAULT-PATHNAME-DEFAULTS*");
	VariableStructImpl<ListStruct> FEATURES_VAR = internAndExportCLVariable("*FEATURES*");
	VariableStructImpl<IntegerStruct> GENSYM_COUNTER_VAR = internAndExportCLVariable("*GENSYM-COUNTER*");
	VariableStructImpl<LispStruct> LOAD_PATHNAME_VAR = internAndExportCLVariable("*LOAD-PATHNAME*");
	VariableStructImpl<BooleanStruct> LOAD_PRINT_VAR = internAndExportCLVariable("*LOAD-PRINT*");
	VariableStructImpl<LispStruct> LOAD_TRUENAME_VAR = internAndExportCLVariable("*LOAD-TRUENAME*");
	VariableStructImpl<BooleanStruct> LOAD_VERBOSE_VAR = internAndExportCLVariable("*LOAD-VERBOSE*");
	VariableStructImpl<FunctionStruct> MACROEXPAND_HOOK_VAR = internAndExportCLVariable("*MACROEXPAND-HOOK*");
	VariableStructImpl<ListStruct> MODULES_VAR = internAndExportCLVariable("*MODULES*");
	VariableStructImpl<PackageStruct> PACKAGE_VAR = internAndExportCLVariable("*PACKAGE*");
	VariableStructImpl<BooleanStruct> PRINT_ARRAY_VAR = internAndExportCLVariable("*PRINT-ARRAY*");
	VariableStructImpl<IntegerStruct> PRINT_BASE_VAR = internAndExportCLVariable("*PRINT-BASE*");
	VariableStructImpl<KeywordStruct> PRINT_CASE_VAR = internAndExportCLVariable("*PRINT-CASE*");
	VariableStructImpl<BooleanStruct> PRINT_CIRCLE_VAR = internAndExportCLVariable("*PRINT-CIRCLE*");
	VariableStructImpl<BooleanStruct> PRINT_ESCAPE_VAR = internAndExportCLVariable("*PRINT-ESCAPE*");
	VariableStructImpl<BooleanStruct> PRINT_GENSYM_VAR = internAndExportCLVariable("*PRINT-GENSYM*");
	VariableStructImpl<IntegerStruct> PRINT_LENGTH_VAR = internAndExportCLVariable("*PRINT-LENGTH*");
	VariableStructImpl<IntegerStruct> PRINT_LEVEL_VAR = internAndExportCLVariable("*PRINT-LEVEL*");
	VariableStructImpl<IntegerStruct> PRINT_LINES_VAR = internAndExportCLVariable("*PRINT-LINES*");
	VariableStructImpl<IntegerStruct> PRINT_MISER_WIDTH_VAR = internAndExportCLVariable("*PRINT-MISER-WIDTH*");
	VariableStructImpl<?> PRINT_PPRINT_DISPATCH_VAR = internAndExportCLVariable("*PRINT-PPRINT-DISPATCH*");
	VariableStructImpl<BooleanStruct> PRINT_PRETTY_VAR = internAndExportCLVariable("*PRINT-PRETTY*");
	VariableStructImpl<BooleanStruct> PRINT_RADIX_VAR = internAndExportCLVariable("*PRINT-RADIX*");
	VariableStructImpl<BooleanStruct> PRINT_READABLY_VAR = internAndExportCLVariable("*PRINT-READABLY*");
	VariableStructImpl<IntegerStruct> PRINT_RIGHT_MARGIN_VAR = internAndExportCLVariable("*PRINT-RIGHT-MARGIN*");
	VariableStructImpl<RandomStateStruct> RANDOM_STATE_VAR = internAndExportCLVariable("*RANDOM-STATE*");
	VariableStructImpl<IntegerStruct> READ_BASE_VAR = internAndExportCLVariable("*READ-BASE*");
	VariableStructImpl<SymbolStruct> READ_DEFAULT_FLOAT_FORMAT_VAR = internAndExportCLVariable("*READ-DEFAULT-FLOAT-FORMAT*");
	VariableStructImpl<BooleanStruct> READ_EVAL_VAR = internAndExportCLVariable("*READ-EVAL*");
	VariableStructImpl<BooleanStruct> READ_SUPPRESS_VAR = internAndExportCLVariable("*READ-SUPPRESS*");
	VariableStructImpl<ReadtableStruct> READTABLE_VAR = internAndExportCLVariable("*READTABLE*");

	SymbolStruct TERMINAL_IO = internAndExportCLSymbol("*TERMINAL-IO*");
	SymbolStruct DEBUG_IO = internAndExportCLSymbol("*DEBUG-IO*");
	SymbolStruct ERROR_OUTPUT = internAndExportCLSymbol("*ERROR-OUTPUT*");
	SymbolStruct QUERY_IO = internAndExportCLSymbol("*QUERY-IO*");
	SymbolStruct STANDARD_INPUT = internAndExportCLSymbol("*STANDARD-INPUT*");
	SymbolStruct STANDARD_OUTPUT = internAndExportCLSymbol("*STANDARD-OUTPUT*");
	SymbolStruct TRACE_OUTPUT = internAndExportCLSymbol("*TRACE-OUTPUT*");

	VariableStructImpl<LispStruct> STAR = internAndExportCLVariable("*");
	VariableStructImpl<LispStruct> STAR_STAR = internAndExportCLVariable("**");
	VariableStructImpl<LispStruct> STAR_STAR_STAR = internAndExportCLVariable("***");
	VariableStructImpl<LispStruct> PLUS = internAndExportCLVariable("+");
	VariableStructImpl<LispStruct> PLUS_PLUS = internAndExportCLVariable("++");
	VariableStructImpl<LispStruct> PLUS_PLUS_PLUS = internAndExportCLVariable("+++");
	VariableStructImpl<LispStruct> DASH = internAndExportCLVariable("-");
	VariableStructImpl<ListStruct> SLASH = internAndExportCLVariable("/");
	VariableStructImpl<ListStruct> SLASH_SLASH = internAndExportCLVariable("//");
	VariableStructImpl<ListStruct> SLASH_SLASH_SLASH = internAndExportCLVariable("///");

	SymbolStruct NOT_EQUAL_SIGN = internAndExportCLSymbol("/=");
	SymbolStruct ONE_PLUS = internAndExportCLSymbol("1+");
	SymbolStruct ONE_MINUS = internAndExportCLSymbol("1-");
	SymbolStruct LESS_THAN_SIGN = internAndExportCLSymbol("<");
	SymbolStruct LESS_THAN_EQUAL_SIGN = internAndExportCLSymbol("<=");
	SymbolStruct EQUAL_SIGN = internAndExportCLSymbol("=");
	SymbolStruct GREATER_THAN_SIGN = internAndExportCLSymbol(">");
	SymbolStruct GREATER_THAN_EQUAL_SIGN = internAndExportCLSymbol(">=");

	SymbolStruct ABORT = internAndExportCLSymbol("ABORT");
	SymbolStruct ABS = internAndExportCLSymbol("ABS");
	SymbolStruct ACONS = internAndExportCLSymbol("ACONS");
	SymbolStruct ACOS = internAndExportCLSymbol("ACOS");
	SymbolStruct ACOSH = internAndExportCLSymbol("ACOSH");
	SymbolStruct ADD_METHOD = internAndExportCLSymbol("ADD-METHOD");
	SymbolStruct ADJOIN = internAndExportCLSymbol("ADJOIN");
	SymbolStruct ADJUST_ARRAY = internAndExportCLSymbol("ADJUST-ARRAY");
	SymbolStruct ADJUSTABLE_ARRAY_P = internAndExportCLSymbol("ADJUSTABLE-ARRAY-P");
	SymbolStruct ALLOCATE_INSTANCE = internAndExportCLSymbol("ALLOCATE-INSTANCE");
	SymbolStruct ALPHA_CHAR_P = internAndExportCLSymbol("ALPHA-CHAR-P");
	SymbolStruct ALPHANUMERICP = internAndExportCLSymbol("ALPHANUMERICP");
	SymbolStruct AND = internAndExportCLSymbol("AND");
	SymbolStruct APPEND = internAndExportCLSymbol("APPEND");
	SymbolStruct APPLY = internAndExportCLSymbol("APPLY");
	SymbolStruct APROPOS = internAndExportCLSymbol("APROPOS");
	SymbolStruct APROPOS_LIST = internAndExportCLSymbol("APROPOS-LIST");
	SymbolStruct AREF = internAndExportCLSymbol("AREF");
	SymbolStruct ARITHMETIC_ERROR = internAndExportCLSymbol("ARITHMETIC-ERROR");
	SymbolStruct ARITHMETIC_ERROR_OPERANDS = internAndExportCLSymbol("ARITHMETIC-ERROR-OPERANDS");
	SymbolStruct ARITHMETIC_ERROR_OPERATION = internAndExportCLSymbol("ARITHMETIC-ERROR-OPERATION");
	SymbolStruct ARRAY = internAndExportCLSymbol("ARRAY");
	SymbolStruct ARRAY_DIMENSION = internAndExportCLSymbol("ARRAY-DIMENSION");
	// ARRAY-DIMENTION-LIMIT
	SymbolStruct ARRAY_DIMENSIONS = internAndExportCLSymbol("ARRAY-DIMENSIONS");
	SymbolStruct ARRAY_DISPLACEMENT = internAndExportCLSymbol("ARRAY-DISPLACEMENT");
	SymbolStruct ARRAY_ELEMENT_TYPE = internAndExportCLSymbol("ARRAY-ELEMENT-TYPE");
	SymbolStruct ARRAY_HAS_FILL_POINTER_P = internAndExportCLSymbol("ARRAY-HAS-FILL-POINTER-P");
	SymbolStruct ARRAY_IN_BOUNDS_P = internAndExportCLSymbol("ARRAY-IN-BOUNDS-P");
	SymbolStruct ARRAY_RANK = internAndExportCLSymbol("ARRAY-RANK");
	// ARRAY-RANK-LIMIT
	SymbolStruct ARRAY_ROW_MAJOR_INDEX = internAndExportCLSymbol("ARRAY-ROW-MAJOR-INDEX");
	SymbolStruct ARRAY_TOTAL_SIZE = internAndExportCLSymbol("ARRAY-TOTAL-SIZE");
	// ARRAY-TOTAL-SIZE-LIMIT
	SymbolStruct ARRAYP = internAndExportCLSymbol("ARRAYP");
	SymbolStruct ASH = internAndExportCLSymbol("ASH");
	SymbolStruct ASIN = internAndExportCLSymbol("ASIN");
	SymbolStruct ASINH = internAndExportCLSymbol("ASINH");
	SymbolStruct ASSERT = internAndExportCLSymbol("ASSERT");
	SymbolStruct ASSOC = internAndExportCLSymbol("ASSOC");
	SymbolStruct ASSOC_IF = internAndExportCLSymbol("ASSOC-IF");
	SymbolStruct ASSOC_IF_NOT = internAndExportCLSymbol("ASSOC-IF-NOT");
	SymbolStruct ATAN = internAndExportCLSymbol("ATAN");
	SymbolStruct ATANH = internAndExportCLSymbol("ATANH");
	SymbolStruct ATOM = internAndExportCLSymbol("ATOM");
	SymbolStruct BASE_CHAR = internAndExportCLSymbol("BASE-CHAR");
	SymbolStruct BASE_STRING = internAndExportCLSymbol("BASE-STRING");
	SymbolStruct BIGNUM = internAndExportCLSymbol("BIGNUM");
	SymbolStruct BIT = internAndExportCLSymbol("BIT");
	SymbolStruct BIT_AND = internAndExportCLSymbol("BIT-AND");
	SymbolStruct BIT_ANDC_1 = internAndExportCLSymbol("BIT-ANDC1");
	SymbolStruct BIT_ANDC_2 = internAndExportCLSymbol("BIT-ANDC2");
	SymbolStruct BIT_EQV = internAndExportCLSymbol("BIT-EQV");
	SymbolStruct BIT_IOR = internAndExportCLSymbol("BIT-IOR");
	SymbolStruct BIT_NAND = internAndExportCLSymbol("BIT-NAND");
	SymbolStruct BIT_NOR = internAndExportCLSymbol("BIT-NOR");
	SymbolStruct BIT_NOT = internAndExportCLSymbol("BIT-NOT");
	SymbolStruct BIT_ORC_1 = internAndExportCLSymbol("BIT-ORC1");
	SymbolStruct BIT_ORC_2 = internAndExportCLSymbol("BIT-ORC2");
	SymbolStruct BIT_VECTOR = internAndExportCLSymbol("BIT-VECTOR");
	SymbolStruct BIT_VECTOR_P = internAndExportCLSymbol("BIT-VECTOR-P");
	SymbolStruct BIT_XOR = internAndExportCLSymbol("BIT-XOR");
	SymbolStruct BLOCK = internAndExportCLSymbol("BLOCK");
	SymbolStruct BOOLE = internAndExportCLSymbol("BOOLE");
	// BOOLE-1
	// BOOLE-2
	// BOOLE-AND
	// BOOLE-ANDC1
	// BOOLE-ANDC2
	// BOOLE-C1
	// BOOLE-C2
	// BOOLE-CLR
	// BOOLE-EQV
	// BOOLE-IOR
	// BOOLE-NAND
	// BOOLE-NOR
	// BOOLE-ORC1
	// BOOLE-ORC2
	// BOOLE-SET
	// BOOLE-XOR
	SymbolStruct BOOLEAN = internAndExportCLSymbol("BOOLEAN");
	SymbolStruct BOTH_CASE_P = internAndExportCLSymbol("BOTH-CASE-P");
	SymbolStruct BOUNDP = internAndExportCLSymbol("BOUNDP");
	SymbolStruct BREAK = internAndExportCLSymbol("BREAK");
	SymbolStruct BROADCAST_STREAM = internAndExportCLSymbol("BROADCAST-STREAM");
	SymbolStruct BROADCAST_STREAM_STREAMS = internAndExportCLSymbol("BROADCAST-STREAM-STREAMS");
	SymbolStruct BUILT_IN_CLASS = internAndExportCLSymbol("BUILT-IN-CLASS");
	SymbolStruct BUTLAST = internAndExportCLSymbol("BUTLAST");
	SymbolStruct BYTE = internAndExportCLSymbol("BYTE");
	SymbolStruct BYTE_POSITION = internAndExportCLSymbol("BYTE-POSITION");
	SymbolStruct BYTE_SIZE = internAndExportCLSymbol("BYTE-SIZE");
	SymbolStruct CAAAAR = internAndExportCLSymbol("CAAAAR");
	SymbolStruct CAAADR = internAndExportCLSymbol("CAAADR");
	SymbolStruct CAAAR = internAndExportCLSymbol("CAAAR");
	SymbolStruct CAADAR = internAndExportCLSymbol("CAADAR");
	SymbolStruct CAADDR = internAndExportCLSymbol("CAADDR");
	SymbolStruct CAADR = internAndExportCLSymbol("CAADR");
	SymbolStruct CAAR = internAndExportCLSymbol("CAAR");
	SymbolStruct CADAAR = internAndExportCLSymbol("CADAAR");
	SymbolStruct CADADR = internAndExportCLSymbol("CADADR");
	SymbolStruct CADAR = internAndExportCLSymbol("CADAR");
	SymbolStruct CADDAR = internAndExportCLSymbol("CADDAR");
	SymbolStruct CADDDR = internAndExportCLSymbol("CADDDR");
	SymbolStruct CADDR = internAndExportCLSymbol("CADDR");
	SymbolStruct CADR = internAndExportCLSymbol("CADR");
	// CALL-ARGUMENTS-LIMIT
	SymbolStruct CALL_METHOD = internAndExportCLSymbol("CALL-METHOD");
	SymbolStruct CALL_NEXT_METHOD = internAndExportCLSymbol("CALL-NEXT-METHOD");
	SymbolStruct CAR = internAndExportCLSymbol("CAR");
	SymbolStruct CASE = internAndExportCLSymbol("CASE");
	SymbolStruct CATCH = internAndExportCLSymbol("CATCH");
	SymbolStruct CCASE = internAndExportCLSymbol("CCASE");
	SymbolStruct CDAAAR = internAndExportCLSymbol("CDAAAR");
	SymbolStruct CDAADR = internAndExportCLSymbol("CDAADR");
	SymbolStruct CDAAR = internAndExportCLSymbol("CDAAR");
	SymbolStruct CDADAR = internAndExportCLSymbol("CDADAR");
	SymbolStruct CDADDR = internAndExportCLSymbol("CDADDR");
	SymbolStruct CDADR = internAndExportCLSymbol("CDADR");
	SymbolStruct CDAR = internAndExportCLSymbol("CDAR");
	SymbolStruct CDDAAR = internAndExportCLSymbol("CDDAAR");
	SymbolStruct CDDADR = internAndExportCLSymbol("CDDADR");
	SymbolStruct CDDAR = internAndExportCLSymbol("CDDAR");
	SymbolStruct CDDDAR = internAndExportCLSymbol("CDDDAR");
	SymbolStruct CDDDDR = internAndExportCLSymbol("CDDDDR");
	SymbolStruct CDDDR = internAndExportCLSymbol("CDDDR");
	SymbolStruct CDDR = internAndExportCLSymbol("CDDR");
	SymbolStruct CDR = internAndExportCLSymbol("CDR");
	SymbolStruct CEILING = internAndExportCLSymbol("CEILING");
	SymbolStruct CELL_ERROR = internAndExportCLSymbol("CELL-ERROR");
	SymbolStruct CELL_ERROR_NAME = internAndExportCLSymbol("CELL-ERROR-NAME");
	SymbolStruct CERROR = internAndExportCLSymbol("CERROR");
	SymbolStruct CHANGE_CLASS = internAndExportCLSymbol("CHANGE-CLASS");
	SymbolStruct CHAR = internAndExportCLSymbol("CHAR");
	SymbolStruct CHAR_CODE = internAndExportCLSymbol("CHAR-CODE");
	// CHAR-CODE-LIMIT
	SymbolStruct CHAR_DOWNCASE = internAndExportCLSymbol("CHAR-DOWNCASE");
	SymbolStruct CHAR_EQUAL = internAndExportCLSymbol("CHAR-EQUAL");
	SymbolStruct CHAR_GREATERP = internAndExportCLSymbol("CHAR-GREATERP");
	SymbolStruct CHAR_INT = internAndExportCLSymbol("CHAR-INT");
	SymbolStruct CHAR_LESSP = internAndExportCLSymbol("CHAR-LESSP");
	SymbolStruct CHAR_NAME = internAndExportCLSymbol("CHAR-NAME");
	SymbolStruct CHAR_NOT_EQUAL = internAndExportCLSymbol("CHAR-NOT-EQUAL");
	SymbolStruct CHAR_NOT_GREATERP = internAndExportCLSymbol("CHAR-NOT-GREATERP");
	SymbolStruct CHAR_NOT_LESSP = internAndExportCLSymbol("CHAR-NOT-LESSP");
	SymbolStruct CHAR_UPCASE = internAndExportCLSymbol("CHAR-UPCASE");
	SymbolStruct CHAR_NOT_EQUAL_CS = internAndExportCLSymbol("CHAR/=");
	SymbolStruct CHAR_LESSP_CS = internAndExportCLSymbol("CHAR<");
	SymbolStruct CHAR_NOT_GREATERP_CS = internAndExportCLSymbol("CHAR<=");
	SymbolStruct CHAR_EQUAL_CS = internAndExportCLSymbol("CHAR=");
	SymbolStruct CHAR_GREATERP_CS = internAndExportCLSymbol("CHAR>");
	SymbolStruct CHAR_NOT_LESSP_CS = internAndExportCLSymbol("CHAR>=");
	SymbolStruct CHARACTER = internAndExportCLSymbol("CHARACTER");
	SymbolStruct CHARACTERP = internAndExportCLSymbol("CHARACTERP");
	SymbolStruct CHECK_TYPE = internAndExportCLSymbol("CHECK-TYPE");
	SymbolStruct CIS = internAndExportCLSymbol("CIS");
	SymbolStruct CLASS = internAndExportCLSymbol("CLASS");
	SymbolStruct CLASS_NAME = internAndExportCLSymbol("CLASS-NAME");
	SymbolStruct CLASS_OF = internAndExportCLSymbol("CLASS-OF");
	SymbolStruct CLEAR_INPUT = internAndExportCLSymbol("CLEAR-INPUT");
	SymbolStruct CLEAR_OUTPUT = internAndExportCLSymbol("CLEAR-OUTPUT");
	SymbolStruct CLOSE = internAndExportCLSymbol("CLOSE");
	SymbolStruct CLRHASH = internAndExportCLSymbol("CLRHASH");
	SymbolStruct CODE_CHAR = internAndExportCLSymbol("CODE-CHAR");
	SymbolStruct COERCE = internAndExportCLSymbol("COERCE");
	// COMPILATION-SPEED
	SymbolStruct COMPILE = internAndExportCLSymbol("COMPILE");
	SymbolStruct COMPILE_FILE = internAndExportCLSymbol("COMPILE-FILE");
	SymbolStruct COMPILE_FILE_PATHNAME = internAndExportCLSymbol("COMPILE-FILE-PATHNAME");
	SymbolStruct COMPILED_FUNCTION = internAndExportCLSymbol("COMPILED-FUNCTION");
	SymbolStruct COMPILED_FUNCTION_P = internAndExportCLSymbol("COMPILED-FUNCTION-P");
	SymbolStruct COMPILER_MACRO = internAndExportCLSymbol("COMPILER-MACRO");
	SymbolStruct COMPILER_MACRO_FUNCTION = internAndExportCLSymbol("COMPILER-MACRO-FUNCTION");
	SymbolStruct COMPLEMENT = internAndExportCLSymbol("COMPLEMENT");
	SymbolStruct COMPLEX = internAndExportCLSymbol("COMPLEX");
	SymbolStruct COMPLEXP = internAndExportCLSymbol("COMPLEXP");
	SymbolStruct COMPUTE_APPLICABLE_METHODS = internAndExportCLSymbol("COMPUTE-APPLICABLE-METHODS");
	SymbolStruct COMPUTE_RESTARTS = internAndExportCLSymbol("COMPUTE-RESTARTS");
	SymbolStruct CONCATENATE = internAndExportCLSymbol("CONCATENATE");
	SymbolStruct CONCATENATED_STREAM = internAndExportCLSymbol("CONCATENATED-STREAM");
	SymbolStruct CONCATENATED_STREAM_STREAMS = internAndExportCLSymbol("CONCATENATED-STREAM-STREAMS");
	SymbolStruct COND = internAndExportCLSymbol("COND");
	SymbolStruct CONDITION = internAndExportCLSymbol("CONDITION");
	SymbolStruct CONJUGATE = internAndExportCLSymbol("CONJUGATE");
	SymbolStruct CONS = internAndExportCLSymbol("CONS");
	SymbolStruct CONSP = internAndExportCLSymbol("CONSP");
	SymbolStruct CONSTANTLY = internAndExportCLSymbol("CONSTANTLY");
	SymbolStruct CONSTANTP = internAndExportCLSymbol("CONSTANTP");
	SymbolStruct CONTINUE = internAndExportCLSymbol("CONTINUE");
	SymbolStruct CONTROL_ERROR = internAndExportCLSymbol("CONTROL-ERROR");
	SymbolStruct COPY_ALIST = internAndExportCLSymbol("COPY-ALIST");
	SymbolStruct COPY_LIST = internAndExportCLSymbol("COPY-LIST");
	SymbolStruct COPY_PPRINT_DISPATCH = internAndExportCLSymbol("COPY-PPRINT-DISPATCH");
	SymbolStruct COPY_READTABLE = internAndExportCLSymbol("COPY-READTABLE");
	SymbolStruct COPY_SEQ = internAndExportCLSymbol("COPY-SEQ");
	SymbolStruct COPY_STRUCTURE = internAndExportCLSymbol("COPY-STRUCTURE");
	SymbolStruct COPY_SYMBOL = internAndExportCLSymbol("COPY-SYMBOL");
	SymbolStruct COPY_TREE = internAndExportCLSymbol("COPY-TREE");
	SymbolStruct COS = internAndExportCLSymbol("COS");
	SymbolStruct COSH = internAndExportCLSymbol("COSH");
	SymbolStruct COUNT = internAndExportCLSymbol("COUNT");
	SymbolStruct COUNT_IF = internAndExportCLSymbol("COUNT-IF");
	SymbolStruct COUNT_IF_NOT = internAndExportCLSymbol("COUNT-IF-NOT");
	SymbolStruct CTYPECASE = internAndExportCLSymbol("CTYPECASE");
	// DEBUG
	SymbolStruct DECF = internAndExportCLSymbol("DECF");
	SymbolStruct DECLAIM = internAndExportCLSymbol("DECLAIM");
	// DECLARATION
	SymbolStruct DECLARE = internAndExportCLSymbol("DECLARE");
	SymbolStruct DECODE_FLOAT = internAndExportCLSymbol("DECODE-FLOAT");
	SymbolStruct DECODE_UNIVERSAL_TIME = internAndExportCLSymbol("DECODE-UNIVERSAL-TIME");
	SymbolStruct DEFCLASS = internAndExportCLSymbol("DEFCLASS");
	SymbolStruct DEFCONSTANT = internAndExportCLSymbol("DEFCONSTANT");
	SymbolStruct DEFGENERIC = internAndExportCLSymbol("DEFGENERIC");
	SymbolStruct DEFINE_COMPILER_MACRO = internAndExportCLSymbol("DEFINE-COMPILER-MACRO");
	SymbolStruct DEFINE_CONDITION = internAndExportCLSymbol("DEFINE-CONDITION");
	SymbolStruct DEFINE_METHOD_COMBINATION = internAndExportCLSymbol("DEFINE-METHOD-COMBINATION");
	SymbolStruct DEFINE_MODIFY_MACRO = internAndExportCLSymbol("DEFINE-MODIFY-MACRO");
	SymbolStruct DEFINE_SETF_EXPANDER = internAndExportCLSymbol("DEFINE-SETF-EXPANDER");
	SymbolStruct DEFINE_SYMBOL_MACRO = internAndExportCLSymbol("DEFINE-SYMBOL-MACRO");
	SymbolStruct DEFMACRO = internAndExportCLSymbol("DEFMACRO");
	SymbolStruct DEFMETHOD = internAndExportCLSymbol("DEFMETHOD");
	SymbolStruct DEFPACKAGE = internAndExportCLSymbol("DEFPACKAGE");
	SymbolStruct DEFPARAMETER = internAndExportCLSymbol("DEFPARAMETER");
	SymbolStruct DEFSETF = internAndExportCLSymbol("DEFSETF");
	SymbolStruct DEFSTRUCT = internAndExportCLSymbol("DEFSTRUCT");
	SymbolStruct DEFTYPE = internAndExportCLSymbol("DEFTYPE");
	SymbolStruct DEFUN = internAndExportCLSymbol("DEFUN");
	SymbolStruct DEFVAR = internAndExportCLSymbol("DEFVAR");
	SymbolStruct DELETE = internAndExportCLSymbol("DELETE");
	SymbolStruct DELETE_DUPLICATES = internAndExportCLSymbol("DELETE-DUPLICATES");
	SymbolStruct DELETE_FILE = internAndExportCLSymbol("DELETE-FILE");
	SymbolStruct DELETE_IF = internAndExportCLSymbol("DELETE-IF");
	SymbolStruct DELETE_IF_NOT = internAndExportCLSymbol("DELETE-IF-NOT");
	SymbolStruct DELETE_PACKAGE = internAndExportCLSymbol("DELETE-PACKAGE");
	SymbolStruct DENOMINATOR = internAndExportCLSymbol("DENOMINATOR");
	SymbolStruct DEPOSIT_FIELD = internAndExportCLSymbol("DEPOSIT-FIELD");
	SymbolStruct DESCRIBE = internAndExportCLSymbol("DESCRIBE");
	SymbolStruct DESCRIBE_OBJECT = internAndExportCLSymbol("DESCRIBE-OBJECT");
	SymbolStruct DESTRUCTURING_BIND = internAndExportCLSymbol("DESTRUCTURING-BIND");
	SymbolStruct DIGIT_CHAR = internAndExportCLSymbol("DIGIT-CHAR");
	SymbolStruct DIGIT_CHAR_P = internAndExportCLSymbol("DIGIT-CHAR-P");
	SymbolStruct DIRECTORY = internAndExportCLSymbol("DIRECTORY");
	SymbolStruct DIRECTORY_NAMESTRING = internAndExportCLSymbol("DIRECTORY-NAMESTRING");
	SymbolStruct DISASSEMBLE = internAndExportCLSymbol("DISASSEMBLE");
	SymbolStruct DIVISION_BY_ZERO = internAndExportCLSymbol("DIVISION-BY-ZERO");
	SymbolStruct DO = internAndExportCLSymbol("DO");
	SymbolStruct DO_STAR = internAndExportCLSymbol("DO*");
	SymbolStruct DO_ALL_SYMBOLS = internAndExportCLSymbol("DO-ALL-SYMBOLS");
	SymbolStruct DO_EXTERNAL_SYMBOLS = internAndExportCLSymbol("DO-EXTERNAL-SYMBOLS");
	SymbolStruct DO_SYMBOLS = internAndExportCLSymbol("DO-SYMBOLS");
	SymbolStruct DOCUMENTATION = internAndExportCLSymbol("DOCUMENTATION");
	SymbolStruct DOLIST = internAndExportCLSymbol("DOLIST");
	SymbolStruct DOTIMES = internAndExportCLSymbol("DOTIMES");
	SymbolStruct DOUBLE_FLOAT = internAndExportCLSymbol("DOUBLE-FLOAT");
	// DOUBLE-FLOAT-EPSILON
	// DOUBLE-FLOAT-NEGATIVE-EPSILON
	SymbolStruct DPB = internAndExportCLSymbol("DPB");
	SymbolStruct DRIBBLE = internAndExportCLSymbol("DRIBBLE");
	// DYNAMIC-EXTENT
	SymbolStruct ECASE = internAndExportCLSymbol("ECASE");
	SymbolStruct ECHO_STREAM = internAndExportCLSymbol("ECHO-STREAM");
	SymbolStruct ECHO_STREAM_INPUT_STREAM = internAndExportCLSymbol("ECHO-STREAM-INPUT-STREAM");
	SymbolStruct ECHO_STREAM_OUTPUT_STREAM = internAndExportCLSymbol("ECHO-STREAM-OUTPUT-STREAM");
	SymbolStruct ED = internAndExportCLSymbol("ED");
	SymbolStruct EIGHTH = internAndExportCLSymbol("EIGHTH");
	SymbolStruct ELT = internAndExportCLSymbol("ELT");
	SymbolStruct ENCODE_UNIVERSAL_TIME = internAndExportCLSymbol("ENCODE-UNIVERSAL-TIME");
	SymbolStruct END_OF_FILE = internAndExportCLSymbol("END-OF-FILE");
	SymbolStruct ENDP = internAndExportCLSymbol("ENDP");
	SymbolStruct ENOUGH_NAMESTRING = internAndExportCLSymbol("ENOUGH-NAMESTRING");
	SymbolStruct ENSURE_DIRECTORIES_EXIST = internAndExportCLSymbol("ENSURE-DIRECTORIES-EXIST");
	SymbolStruct ENSURE_GENERIC_FUNCTION = internAndExportCLSymbol("ENSURE-GENERIC-FUNCTION");
	SymbolStruct EQ = internAndExportCLSymbol("EQ");
	SymbolStruct EQL = internAndExportCLSymbol("EQL");
	SymbolStruct EQUAL = internAndExportCLSymbol("EQUAL");
	SymbolStruct EQUALP = internAndExportCLSymbol("EQUALP");
	SymbolStruct ERROR = internAndExportCLSymbol("ERROR");
	SymbolStruct ETYPECASE = internAndExportCLSymbol("ETYPECASE");
	SymbolStruct EVAL = internAndExportCLSymbol("EVAL");
	SymbolStruct EVAL_WHEN = internAndExportCLSymbol("EVAL-WHEN");
	SymbolStruct EVENP = internAndExportCLSymbol("EVENP");
	SymbolStruct EVERY = internAndExportCLSymbol("EVERY");
	SymbolStruct EXP = internAndExportCLSymbol("EXP");
	SymbolStruct EXPORT = internAndExportCLSymbol("EXPORT");
	SymbolStruct EXPT = internAndExportCLSymbol("EXPT");
	SymbolStruct EXTENDED_CHAR = internAndExportCLSymbol("EXTENDED-CHAR");
	SymbolStruct FBOUNDP = internAndExportCLSymbol("FBOUNDP");
	SymbolStruct FCEILING = internAndExportCLSymbol("FCEILING");
	SymbolStruct FDEFINITION = internAndExportCLSymbol("FDEFINITION");
	SymbolStruct FFLOOR = internAndExportCLSymbol("FFLOOR");
	SymbolStruct FIFTH = internAndExportCLSymbol("FIFTH");
	SymbolStruct FILE_AUTHOR = internAndExportCLSymbol("FILE-AUTHOR");
	SymbolStruct FILE_ERROR = internAndExportCLSymbol("FILE-ERROR");
	SymbolStruct FILE_ERROR_PATHNAME = internAndExportCLSymbol("FILE-ERROR-PATHNAME");
	SymbolStruct FILE_LENGTH = internAndExportCLSymbol("FILE-LENGTH");
	SymbolStruct FILE_NAMESTRING = internAndExportCLSymbol("FILE-NAMESTRING");
	SymbolStruct FILE_POSITION = internAndExportCLSymbol("FILE-POSITION");
	SymbolStruct FILE_STREAM = internAndExportCLSymbol("FILE-STREAM");
	SymbolStruct FILE_STRING_LENGTH = internAndExportCLSymbol("FILE-STRING-LENGTH");
	SymbolStruct FILE_WRITE_DATE = internAndExportCLSymbol("FILE-WRITE-DATE");
	SymbolStruct FILL = internAndExportCLSymbol("FILL");
	SymbolStruct FILL_POINTER = internAndExportCLSymbol("FILL-POINTER");
	SymbolStruct FIND = internAndExportCLSymbol("FIND");
	SymbolStruct FIND_ALL_SYMBOLS = internAndExportCLSymbol("FIND-ALL-SYMBOLS");
	SymbolStruct FIND_CLASS = internAndExportCLSymbol("FIND-CLASS");
	SymbolStruct FIND_IF = internAndExportCLSymbol("FIND-IF");
	SymbolStruct FIND_IF_NOT = internAndExportCLSymbol("FIND-IF-NOT");
	SymbolStruct FIND_METHOD = internAndExportCLSymbol("FIND-METHOD");
	SymbolStruct FIND_PACKAGE = internAndExportCLSymbol("FIND-PACKAGE");
	SymbolStruct FIND_RESTART = internAndExportCLSymbol("FIND-RESTART");
	SymbolStruct FIND_SYMBOL = internAndExportCLSymbol("FIND-SYMBOL");
	SymbolStruct FINISH_OUTPUT = internAndExportCLSymbol("FINISH-OUTPUT");
	SymbolStruct FIRST = internAndExportCLSymbol("FIRST");
	SymbolStruct FIXNUM = internAndExportCLSymbol("FIXNUM");
	SymbolStruct FLET = internAndExportCLSymbol("FLET");
	SymbolStruct FLOAT = internAndExportCLSymbol("FLOAT");
	SymbolStruct FLOAT_DIGITS = internAndExportCLSymbol("FLOAT-DIGITS");
	SymbolStruct FLOAT_PRECISION = internAndExportCLSymbol("FLOAT-PRECISION");
	SymbolStruct FLOAT_RADIX = internAndExportCLSymbol("FLOAT-RADIX");
	SymbolStruct FLOAT_SIGN = internAndExportCLSymbol("FLOAT-SIGN");
	SymbolStruct FLOATING_POINT_INEXACT = internAndExportCLSymbol("FLOATING-POINT-INEXACT");
	SymbolStruct FLOATING_POINT_INVALID_OPERATION = internAndExportCLSymbol("FLOATING-POINT-INVALID-OPERATION");
	SymbolStruct FLOATING_POINT_OVERFLOW = internAndExportCLSymbol("FLOATING-POINT-OVERFLOW");
	SymbolStruct FLOATING_POINT_UNDERFLOW = internAndExportCLSymbol("FLOATING-POINT-UNDERFLOW");
	SymbolStruct FLOATP = internAndExportCLSymbol("FLOATP");
	SymbolStruct FLOOR = internAndExportCLSymbol("FLOOR");
	SymbolStruct FMAKUNBOUND = internAndExportCLSymbol("FMAKUNBOUND");
	SymbolStruct FORCE_OUTPUT = internAndExportCLSymbol("FORCE-OUTPUT");
	SymbolStruct FORMAT = internAndExportCLSymbol("FORMAT");
	SymbolStruct FORMATTER = internAndExportCLSymbol("FORMATTER");
	SymbolStruct FOURTH = internAndExportCLSymbol("FOURTH");
	SymbolStruct FRESH_LINE = internAndExportCLSymbol("FRESH-LINE");
	SymbolStruct FROUND = internAndExportCLSymbol("FROUND");
	SymbolStruct FTRUNCATE = internAndExportCLSymbol("FTRUNCATE");
	// FTYPE
	SymbolStruct FUNCALL = internAndExportCLSymbol("FUNCALL");
	SymbolStruct FUNCTION = internAndExportCLSymbol("FUNCTION");
	SymbolStruct FUNCTION_KEYWORDS = internAndExportCLSymbol("FUNCTION-KEYWORDS");
	SymbolStruct FUNCTION_LAMBDA_EXPRESSION = internAndExportCLSymbol("FUNCTION-LAMBDA-EXPRESSION");
	SymbolStruct FUNCTIONP = internAndExportCLSymbol("FUNCTIONP");
	SymbolStruct GCD = internAndExportCLSymbol("GCD");
	SymbolStruct GENERIC_FUNCTION = internAndExportCLSymbol("GENERIC-FUNCTION");
	SymbolStruct GENSYM = internAndExportCLSymbol("GENSYM");
	SymbolStruct GENTEMP = internAndExportCLSymbol("GENTEMP");
	SymbolStruct GET = internAndExportCLSymbol("GET");
	SymbolStruct GET_DECODED_TIME = internAndExportCLSymbol("GET-DECODED-TIME");
	SymbolStruct GET_DISPATCH_MACRO_CHARACTER = internAndExportCLSymbol("GET-DISPATCH-MACRO-CHARACTER");
	SymbolStruct GET_INTERNAL_REAL_TIME = internAndExportCLSymbol("GET-INTERNAL-REAL-TIME");
	SymbolStruct GET_INTERNAL_RUN_TIME = internAndExportCLSymbol("GET-INTERNAL-RUN-TIME");
	SymbolStruct GET_MACRO_CHARACTER = internAndExportCLSymbol("GET-MACRO-CHARACTER");
	SymbolStruct GET_OUTPUT_STREAM_STRING = internAndExportCLSymbol("GET-OUTPUT-STREAM-STRING");
	SymbolStruct GET_PROPERTIES = internAndExportCLSymbol("GET-PROPERTIES");
	SymbolStruct GET_SETF_EXPANSION = internAndExportCLSymbol("GET-SETF-EXPANSION");
	SymbolStruct GET_UNIVERSAL_TIME = internAndExportCLSymbol("GET-UNIVERSAL-TIME");
	SymbolStruct GETF = internAndExportCLSymbol("GETF");
	SymbolStruct GETHASH = internAndExportCLSymbol("GETHASH");
	SymbolStruct GO = internAndExportCLSymbol("GO");
	SymbolStruct GRAPHIC_CHAR_P = internAndExportCLSymbol("GRAPHIC-CHAR-P");
	SymbolStruct HANDLER_BIND = internAndExportCLSymbol("HANDLER-BIND");
	SymbolStruct HANDLER_CASE = internAndExportCLSymbol("HANDLER-CASE");
	SymbolStruct HASH_TABLE = internAndExportCLSymbol("HASH-TABLE");
	SymbolStruct HASH_TABLE_COUNT = internAndExportCLSymbol("HASH-TABLE-COUNT");
	SymbolStruct HASH_TABLE_P = internAndExportCLSymbol("HASH-TABLE-P");
	SymbolStruct HASH_TABLE_REHASH_SIZE = internAndExportCLSymbol("HASH-TABLE-REHASH-SIZE");
	SymbolStruct HASH_TABLE_REHASH_THRESHOLD = internAndExportCLSymbol("HASH-TABLE-REHASH-THRESHOLD");
	SymbolStruct HASH_TABLE_SIZE = internAndExportCLSymbol("HASH-TABLE-SIZE");
	SymbolStruct HASH_TABLE_TEST = internAndExportCLSymbol("HASH-TABLE-TEST");
	SymbolStruct HOST_NAMESTRING = internAndExportCLSymbol("HOST-NAMESTRING");
	SymbolStruct IDENTITY = internAndExportCLSymbol("IDENTITY");
	SymbolStruct IF = internAndExportCLSymbol("IF");
	// IGNORABLE
	// IGNORE
	SymbolStruct IGNORE_ERRORS = internAndExportCLSymbol("IGNORE-ERRORS");
	SymbolStruct IMAGPART = internAndExportCLSymbol("IMAGPART");
	SymbolStruct IMPORT = internAndExportCLSymbol("IMPORT");
	SymbolStruct IN_PACKAGE = internAndExportCLSymbol("IN-PACKAGE");
	SymbolStruct INCF = internAndExportCLSymbol("INCF");
	SymbolStruct INITIALIZE_INSTANCE = internAndExportCLSymbol("INITIALIZE-INSTANCE");
	// INLINE
	SymbolStruct INPUT_STREAM_P = internAndExportCLSymbol("INPUT-STREAM-P");
	SymbolStruct INSPECT = internAndExportCLSymbol("INSPECT");
	SymbolStruct INTEGER = internAndExportCLSymbol("INTEGER");
	SymbolStruct INTEGER_DECODE_FLOAT = internAndExportCLSymbol("INTEGER-DECODE-FLOAT");
	SymbolStruct INTEGER_LENGTH = internAndExportCLSymbol("INTEGER-LENGTH");
	SymbolStruct INTEGERP = internAndExportCLSymbol("INTEGERP");
	SymbolStruct INTERACTIVE_STREAM_P = internAndExportCLSymbol("INTERACTIVE-STREAM-P");
	SymbolStruct INTERN = internAndExportCLSymbol("INTERN");
	// INTERNAL-TIME-UNITS-PER-SECOND
	SymbolStruct INTERSECTION = internAndExportCLSymbol("INTERSECTION");
	SymbolStruct INVALID_METHOD_ERROR = internAndExportCLSymbol("INVALID-METHOD-ERROR");
	SymbolStruct INVOKE_DEBUGGER = internAndExportCLSymbol("INVOKE-DEBUGGER");
	SymbolStruct INVOKE_RESTART = internAndExportCLSymbol("INVOKE-RESTART");
	SymbolStruct INVOKE_RESTART_INTERACTIVELY = internAndExportCLSymbol("INVOKE-RESTART-INTERACTIVELY");
	SymbolStruct ISQRT = internAndExportCLSymbol("ISQRT");
	SymbolStruct KEYWORD = internAndExportCLSymbol("KEYWORD");
	SymbolStruct KEYWORDP = internAndExportCLSymbol("KEYWORDP");
	SymbolStruct LABELS = internAndExportCLSymbol("LABELS");
	SymbolStruct LAMBDA = internAndExportCLSymbol("LAMBDA");
	// LAMBDA-LIST-KEYWORDS
	// LAMBDA-PARAMETERS-LIMIT
	SymbolStruct LAST = internAndExportCLSymbol("LAST");
	SymbolStruct LCM = internAndExportCLSymbol("LCM");
	SymbolStruct LDB = internAndExportCLSymbol("LDB");
	SymbolStruct LDB_TEST = internAndExportCLSymbol("LDB-TEST");
	SymbolStruct LDIFF = internAndExportCLSymbol("LDIFF");
	// LEAST-NEGATIVE-DOUBLE-FLOAT
	// LEAST-NEGATIVE-LONG-FLOAT
	// LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
	// LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
	// LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
	// LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
	// LEAST-NEGATIVE-SHORT-FLOAT
	// LEAST-NEGATIVE-SINGLE-FLOAT
	// LEAST-POSITIVE-DOUBLE-FLOAT
	// LEAST-POSITIVE-LONG-FLOAT
	// LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
	// LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
	// LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
	// LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
	// LEAST-POSITIVE-SHORT-FLOAT
	// LEAST-POSITIVE-SINGLE-FLOAT
	SymbolStruct LENGTH = internAndExportCLSymbol("LENGTH");
	SymbolStruct LET = internAndExportCLSymbol("LET");
	SymbolStruct LET_STAR = internAndExportCLSymbol("LET*");
	SymbolStruct LISP_IMPLEMENTATION_TYPE = internAndExportCLSymbol("LISP-IMPLEMENTATION-TYPE");
	SymbolStruct LISP_IMPLEMENTATION_VERSION = internAndExportCLSymbol("LISP-IMPLEMENTATION-VERSION");
	SymbolStruct LIST = internAndExportCLSymbol("LIST");
	SymbolStruct LIST_STAR = internAndExportCLSymbol("LIST*");
	SymbolStruct LIST_ALL_PACKAGES = internAndExportCLSymbol("LIST-ALL-PACKAGES");
	SymbolStruct LIST_LENGTH = internAndExportCLSymbol("LIST-LENGTH");
	SymbolStruct LISTEN = internAndExportCLSymbol("LISTEN");
	SymbolStruct LISTP = internAndExportCLSymbol("LISTP");
	SymbolStruct LOAD = internAndExportCLSymbol("LOAD");
	SymbolStruct LOAD_LOGICAL_PATHNAME_TRANSLATIONS = internAndExportCLSymbol("LOAD-LOGICAL-PATHNAME-TRANSLATIONS");
	SymbolStruct LOAD_TIME_VALUE = internAndExportCLSymbol("LOAD-TIME-VALUE");
	SymbolStruct LOCALLY = internAndExportCLSymbol("LOCALLY");
	SymbolStruct LOG = internAndExportCLSymbol("LOG");
	SymbolStruct LOGAND = internAndExportCLSymbol("LOGAND");
	SymbolStruct LOGANDC1 = internAndExportCLSymbol("LOGANDC1");
	SymbolStruct LOGANDC2 = internAndExportCLSymbol("LOGANDC2");
	SymbolStruct LOGBITP = internAndExportCLSymbol("LOGBITP");
	SymbolStruct LOGCOUNT = internAndExportCLSymbol("LOGCOUNT");
	SymbolStruct LOGEQV = internAndExportCLSymbol("LOGEQV");
	SymbolStruct LOGICAL_PATHNAME = internAndExportCLSymbol("LOGICAL-PATHNAME");
	SymbolStruct LOGICAL_PATHNAME_TRANSLATIONS = internAndExportCLSymbol("LOGICAL-PATHNAME-TRANSLATIONS");
	SymbolStruct LOGIOR = internAndExportCLSymbol("LOGIOR");
	SymbolStruct LOGNAND = internAndExportCLSymbol("LOGNAND");
	SymbolStruct LOGNOR = internAndExportCLSymbol("LOGNOR");
	SymbolStruct LOGNOT = internAndExportCLSymbol("LOGNOT");
	SymbolStruct LOGORC1 = internAndExportCLSymbol("LOGORC1");
	SymbolStruct LOGORC2 = internAndExportCLSymbol("LOGORC2");
	SymbolStruct LOGTEST = internAndExportCLSymbol("LOGTEST");
	SymbolStruct LOGXOR = internAndExportCLSymbol("LOGXOR");
	SymbolStruct LONG_FLOAT = internAndExportCLSymbol("LONG-FLOAT");
	// LONG-FLOAT-EPSILON
	// LONG-FLOAT-NEGATIVE-EPSILON
	SymbolStruct LONG_SITE_NAME = internAndExportCLSymbol("LONG-SITE-NAME");
	SymbolStruct LOOP = internAndExportCLSymbol("LOOP");
	SymbolStruct LOOP_FINISH = internAndExportCLSymbol("LOOP-FINISH");
	SymbolStruct LOWER_CASE_P = internAndExportCLSymbol("LOWER-CASE-P");
	SymbolStruct MACHINE_INSTANCE = internAndExportCLSymbol("MACHINE-INSTANCE");
	SymbolStruct MACHINE_TYPE = internAndExportCLSymbol("MACHINE-TYPE");
	SymbolStruct MACHINE_VERSION = internAndExportCLSymbol("MACHINE-VERSION");
	SymbolStruct MACRO_FUNCTION = internAndExportCLSymbol("MACRO-FUNCTION");
	SymbolStruct MACROEXPAND = internAndExportCLSymbol("MACROEXPAND");
	SymbolStruct MACROEXPAND_1 = internAndExportCLSymbol("MACROEXPAND-1");
	SymbolStruct MACROLET = internAndExportCLSymbol("MACROLET");
	SymbolStruct MAKE_ARRAY = internAndExportCLSymbol("MAKE-ARRAY");
	SymbolStruct MAKE_BROADCAST_STREAM = internAndExportCLSymbol("MAKE-BROADCAST-STREAM");
	SymbolStruct MAKE_CONCATENATED_STREAM = internAndExportCLSymbol("MAKE-CONCATENATED-STREAM");
	SymbolStruct MAKE_CONDITION = internAndExportCLSymbol("MAKE-CONDITION");
	SymbolStruct MAKE_DISPATCH_MACRO_CHARACTER = internAndExportCLSymbol("MAKE-DISPATCH-MACRO-CHARACTER");
	SymbolStruct MAKE_ECHO_STREAM = internAndExportCLSymbol("MAKE-ECHO-STREAM");
	SymbolStruct MAKE_HASH_TABLE = internAndExportCLSymbol("MAKE-HASH-TABLE");
	SymbolStruct MAKE_INSTANCE = internAndExportCLSymbol("MAKE-INSTANCE");
	SymbolStruct MAKE_INSTANCES_OBSOLETE = internAndExportCLSymbol("MAKE-INSTANCES-OBSOLETE");
	SymbolStruct MAKE_LIST = internAndExportCLSymbol("MAKE-LIST");
	SymbolStruct MAKE_LOAD_FORM = internAndExportCLSymbol("MAKE-LOAD-FORM");
	SymbolStruct MAKE_LOAD_FORM_SAVING_SLOTS = internAndExportCLSymbol("MAKE-LOAD-FORM-SAVING-SLOTS");
	SymbolStruct MAKE_METHOD = internAndExportCLSymbol("MAKE-METHOD");
	SymbolStruct MAKE_PACKAGE = internAndExportCLSymbol("MAKE-PACKAGE");
	SymbolStruct MAKE_PATHNAME = internAndExportCLSymbol("MAKE-PATHNAME");
	SymbolStruct MAKE_RANDOM_STATE = internAndExportCLSymbol("MAKE-RANDOM-STATE");
	SymbolStruct MAKE_SEQUENCE = internAndExportCLSymbol("MAKE-SEQUENCE");
	SymbolStruct MAKE_STRING = internAndExportCLSymbol("MAKE-STRING");
	SymbolStruct MAKE_STRING_INPUT_STREAM = internAndExportCLSymbol("MAKE-STRING-INPUT-STREAM");
	SymbolStruct MAKE_STRING_OUTPUT_STREAM = internAndExportCLSymbol("MAKE-STRING-OUTPUT-STREAM");
	SymbolStruct MAKE_SYMBOL = internAndExportCLSymbol("MAKE-SYMBOL");
	SymbolStruct MAKE_SYNONYM_STREAM = internAndExportCLSymbol("MAKE-SYNONYM-STREAM");
	SymbolStruct MAKE_TWO_WAY_STREAM = internAndExportCLSymbol("MAKE-TWO-WAY-STREAM");
	SymbolStruct MAKUNBOUND = internAndExportCLSymbol("MAKUNBOUND");
	SymbolStruct MAP = internAndExportCLSymbol("MAP");
	SymbolStruct MAP_INTO = internAndExportCLSymbol("MAP-INTO");
	SymbolStruct MAPC = internAndExportCLSymbol("MAPC");
	SymbolStruct MAPCAN = internAndExportCLSymbol("MAPCAN");
	SymbolStruct MAPCAR = internAndExportCLSymbol("MAPCAR");
	SymbolStruct MAPCON = internAndExportCLSymbol("MAPCON");
	SymbolStruct MAPHASH = internAndExportCLSymbol("MAPHASH");
	SymbolStruct MAPL = internAndExportCLSymbol("MAPL");
	SymbolStruct MAPLIST = internAndExportCLSymbol("MAPLIST");
	SymbolStruct MASK_FIELD = internAndExportCLSymbol("MASK-FIELD");
	SymbolStruct MAX = internAndExportCLSymbol("MAX");
	SymbolStruct MEMBER = internAndExportCLSymbol("MEMBER");
	SymbolStruct MEMBER_IF = internAndExportCLSymbol("MEMBER-IF");
	SymbolStruct MEMBER_IF_NOT = internAndExportCLSymbol("MEMBER-IF-NOT");
	SymbolStruct MERGE = internAndExportCLSymbol("MERGE");
	SymbolStruct MERGE_PATHNAMES = internAndExportCLSymbol("MERGE-PATHNAMES");
	SymbolStruct METHOD = internAndExportCLSymbol("METHOD");
	SymbolStruct METHOD_COMBINATION = internAndExportCLSymbol("METHOD-COMBINATION");
	SymbolStruct METHOD_COMBINATION_ERROR = internAndExportCLSymbol("METHOD-COMBINATION-ERROR");
	SymbolStruct METHOD_QUALIFIERS = internAndExportCLSymbol("METHOD-QUALIFIERS");
	SymbolStruct MIN = internAndExportCLSymbol("MIN");
	SymbolStruct MINUSP = internAndExportCLSymbol("MINUSP");
	SymbolStruct MISMATCH = internAndExportCLSymbol("MISMATCH");
	SymbolStruct MOD = internAndExportCLSymbol("MOD");
	// MOST-NEGATIVE-DOUBLE-FLOAT
	// MOST-NEGATIVE-FIXNUM
	// MOST-NEGATIVE-LONG-FLOAT
	// MOST-NEGATIVE-SHORT-FLOAT
	// MOST-NEGATIVE-SINGLE-FLOAT
	// MOST-POSITIVE-DOUBLE-FLOAT
	// MOST-POSITIVE-FIXNUM
	// MOST-POSITIVE-LONG-FLOAT
	// MOST-POSITIVE-SHORT-FLOAT
	// MOST-POSITIVE-SINGLE-FLOAT
	SymbolStruct MUFFLE_WARNING = internAndExportCLSymbol("MUFFLE-WARNING");
	SymbolStruct MULTIPLE_VALUE_BIND = internAndExportCLSymbol("MULTIPLE-VALUE-BIND");
	SymbolStruct MULTIPLE_VALUE_CALL = internAndExportCLSymbol("MULTIPLE-VALUE-CALL");
	SymbolStruct MULTIPLE_VALUE_LIST = internAndExportCLSymbol("MULTIPLE-VALUE-LIST");
	SymbolStruct MULTIPLE_VALUE_PROG1 = internAndExportCLSymbol("MULTIPLE-VALUE-PROG1");
	SymbolStruct MULTIPLE_VALUE_SETQ = internAndExportCLSymbol("MULTIPLE-VALUE-SETQ");
	// MULTIPLE-VALUES-LIMIT
	SymbolStruct NAME_CHAR = internAndExportCLSymbol("NAME-CHAR");
	SymbolStruct NAMESTRING = internAndExportCLSymbol("NAMESTRING");
	SymbolStruct NBUTLAST = internAndExportCLSymbol("NBUTLAST");
	SymbolStruct NCONC = internAndExportCLSymbol("NCONC");
	SymbolStruct NEXT_METHOD_P = internAndExportCLSymbol("NEXT-METHOD-P");
	// NIL
	SymbolStruct NINTERSECTION = internAndExportCLSymbol("NINTERSECTION");
	SymbolStruct NINTH = internAndExportCLSymbol("NINTH");
	SymbolStruct NO_APPLICABLE_METHOD = internAndExportCLSymbol("NO-APPLICABLE-METHOD");
	SymbolStruct NO_NEXT_METHOD = internAndExportCLSymbol("NO-NEXT-METHOD");
	SymbolStruct NOT = internAndExportCLSymbol("NOT");
	SymbolStruct NOTANY = internAndExportCLSymbol("NOTANY");
	SymbolStruct NOTEVERY = internAndExportCLSymbol("NOTEVERY");
	// NOTINLINE
	SymbolStruct NRECONC = internAndExportCLSymbol("NRECONC");
	SymbolStruct NREVERSE = internAndExportCLSymbol("NREVERSE");
	SymbolStruct NSET_DIFFERENCE = internAndExportCLSymbol("NSET-DIFFERENCE");
	SymbolStruct NSET_EXCLUSIVE_OR = internAndExportCLSymbol("NSET-EXCLUSIVE-OR");
	SymbolStruct NSTRING_CAPITALIZE = internAndExportCLSymbol("NSTRING-CAPITALIZE");
	SymbolStruct NSTRING_DOWNCASE = internAndExportCLSymbol("NSTRING-DOWNCASE");
	SymbolStruct NSTRING_UPCASE = internAndExportCLSymbol("NSTRING-UPCASE");
	SymbolStruct NSUBLIS = internAndExportCLSymbol("NSUBLIS");
	SymbolStruct NSUBST = internAndExportCLSymbol("NSUBST");
	SymbolStruct NSUBST_IF = internAndExportCLSymbol("NSUBST-IF");
	SymbolStruct NSUBST_IF_NOT = internAndExportCLSymbol("NSUBST-IF-NOT");
	SymbolStruct NSUBSTITUTE = internAndExportCLSymbol("NSUBSTITUTE");
	SymbolStruct NSUBSTITUTE_IF = internAndExportCLSymbol("NSUBSTITUTE-IF");
	SymbolStruct NSUBSTITUTE_IF_NOT = internAndExportCLSymbol("NSUBSTITUTE-IF-NOT");
	SymbolStruct NTH = internAndExportCLSymbol("NTH");
	SymbolStruct NTH_VALUE = internAndExportCLSymbol("NTH-VALUE");
	SymbolStruct NTHCDR = internAndExportCLSymbol("NTHCDR");
	SymbolStruct NULL = internAndExportCLSymbol("NULL");
	SymbolStruct NUMBER = internAndExportCLSymbol("NUMBER");
	SymbolStruct NUMBERP = internAndExportCLSymbol("NUMBERP");
	SymbolStruct NUMERATOR = internAndExportCLSymbol("NUMERATOR");
	SymbolStruct NUNION = internAndExportCLSymbol("NUNION");
	SymbolStruct ODDP = internAndExportCLSymbol("ODDP");
	SymbolStruct OPEN = internAndExportCLSymbol("OPEN");
	SymbolStruct OPEN_STREAM_P = internAndExportCLSymbol("OPEN-STREAM-P");
	// OPTIMIZE
	SymbolStruct OR = internAndExportCLSymbol("OR");
	SymbolStruct OTHERWISE = internAndExportCLSymbol("OTHERWISE");
	SymbolStruct OUTPUT_STREAM_P = internAndExportCLSymbol("OUTPUT-STREAM-P");
	SymbolStruct PACKAGE = internAndExportCLSymbol("PACKAGE");
	SymbolStruct PACKAGE_ERROR = internAndExportCLSymbol("PACKAGE-ERROR");
	SymbolStruct PACKAGE_ERROR_PACKAGE = internAndExportCLSymbol("PACKAGE-ERROR-PACKAGE");
	SymbolStruct PACKAGE_NAME = internAndExportCLSymbol("PACKAGE-NAME");
	SymbolStruct PACKAGE_NICKNAMES = internAndExportCLSymbol("PACKAGE-NICKNAMES");
	SymbolStruct PACKAGE_SHADOWING_SYMBOLS = internAndExportCLSymbol("PACKAGE-SHADOWING-SYMBOLS");
	SymbolStruct PACKAGE_USE_LIST = internAndExportCLSymbol("PACKAGE-USE-LIST");
	SymbolStruct PACKAGE_USED_BY_LIST = internAndExportCLSymbol("PACKAGE-USED-BY-LIST");
	SymbolStruct PACKAGEP = internAndExportCLSymbol("PACKAGEP");
	SymbolStruct PAIRLIS = internAndExportCLSymbol("PAIRLIS");
	SymbolStruct PARSE_ERROR = internAndExportCLSymbol("PARSE-ERROR");
	SymbolStruct PARSE_INTEGER = internAndExportCLSymbol("PARSE-INTEGER");
	SymbolStruct PARSE_NAMESTRING = internAndExportCLSymbol("PARSE-NAMESTRING");
	SymbolStruct PATHNAME = internAndExportCLSymbol("PATHNAME");
	SymbolStruct PATHNAME_DEVICE = internAndExportCLSymbol("PATHNAME-DEVICE");
	SymbolStruct PATHNAME_DIRECTORY = internAndExportCLSymbol("PATHNAME-DIRECTORY");
	SymbolStruct PATHNAME_HOST = internAndExportCLSymbol("PATHNAME-HOST");
	SymbolStruct PATHNAME_MATCH_P = internAndExportCLSymbol("PATHNAME-MATCH-P");
	SymbolStruct PATHNAME_NAME = internAndExportCLSymbol("PATHNAME-NAME");
	SymbolStruct PATHNAME_TYPE = internAndExportCLSymbol("PATHNAME-TYPE");
	SymbolStruct PATHNAME_VERSION = internAndExportCLSymbol("PATHNAME-VERSION");
	SymbolStruct PATHNAMEP = internAndExportCLSymbol("PATHNAMEP");
	SymbolStruct PEEK_CHAR = internAndExportCLSymbol("PEEK-CHAR");
	SymbolStruct PHASE = internAndExportCLSymbol("PHASE");
	// PI
	SymbolStruct PLUSP = internAndExportCLSymbol("PLUSP");
	SymbolStruct POP = internAndExportCLSymbol("POP");
	SymbolStruct POSITION = internAndExportCLSymbol("POSITION");
	SymbolStruct POSITION_IF = internAndExportCLSymbol("POSITION-IF");
	SymbolStruct POSITION_IF_NOT = internAndExportCLSymbol("POSITION-IF-NOT");
	SymbolStruct PPRINT = internAndExportCLSymbol("PPRINT");
	SymbolStruct PPRINT_DISPATCH = internAndExportCLSymbol("PPRINT-DISPATCH");
	SymbolStruct PPRINT_EXIT_IF_LIST_EXHAUSTED = internAndExportCLSymbol("PPRINT-EXIT-IF-LIST-EXHAUSTED");
	SymbolStruct PPRINT_FILL = internAndExportCLSymbol("PPRINT-FILL");
	SymbolStruct PPRINT_INDENT = internAndExportCLSymbol("PPRINT-INDENT");
	SymbolStruct PPRINT_LINEAR = internAndExportCLSymbol("PPRINT-LINEAR");
	SymbolStruct PPRINT_LOGICAL_BLOCK = internAndExportCLSymbol("PPRINT-LOGICAL-BLOCK");
	SymbolStruct PPRINT_NEWLINE = internAndExportCLSymbol("PPRINT-NEWLINE");
	SymbolStruct PPRINT_POP = internAndExportCLSymbol("PPRINT-POP");
	SymbolStruct PPRINT_TAB = internAndExportCLSymbol("PPRINT-TAB");
	SymbolStruct PPRINT_TABULAR = internAndExportCLSymbol("PPRINT-TABULAR");
	SymbolStruct PRIN1 = internAndExportCLSymbol("PRIN1");
	SymbolStruct PRIN1_TO_STRING = internAndExportCLSymbol("PRIN1-TO-STRING");
	SymbolStruct PRINC = internAndExportCLSymbol("PRINC");
	SymbolStruct PRINC_TO_STRING = internAndExportCLSymbol("PRINC-TO-STRING");
	SymbolStruct PRINT = internAndExportCLSymbol("PRINT");
	SymbolStruct PRINT_NOT_READABLE = internAndExportCLSymbol("PRINT-NOT-READABLE");
	SymbolStruct PRINT_NOT_READABLE_OBJECT = internAndExportCLSymbol("PRINT-NOT-READABLE-OBJECT");
	SymbolStruct PRINT_OBJECT = internAndExportCLSymbol("PRINT-OBJECT");
	SymbolStruct PRINT_UNREADABLE_OBJECT = internAndExportCLSymbol("PRINT-UNREADABLE-OBJECT");
	SymbolStruct PROBE_FILE = internAndExportCLSymbol("PROBE-FILE");
	SymbolStruct PROCLAIM = internAndExportCLSymbol("PROCLAIM");
	SymbolStruct PROG = internAndExportCLSymbol("PROG");
	SymbolStruct PROG_STAR = internAndExportCLSymbol("PROG*");
	SymbolStruct PROG1 = internAndExportCLSymbol("PROG1");
	SymbolStruct PROG2 = internAndExportCLSymbol("PROG2");
	SymbolStruct PROGN = internAndExportCLSymbol("PROGN");
	SymbolStruct PROGRAM_ERROR = internAndExportCLSymbol("PROGRAM-ERROR");
	SymbolStruct PROGV = internAndExportCLSymbol("PROGV");
	SymbolStruct PROVIDE = internAndExportCLSymbol("PROVIDE");
	SymbolStruct PSETF = internAndExportCLSymbol("PSETF");
	SymbolStruct PSETQ = internAndExportCLSymbol("PSETQ");
	SymbolStruct PUSH = internAndExportCLSymbol("PUSH");
	SymbolStruct PUSHNEW = internAndExportCLSymbol("PUSHNEW");
	SymbolStruct QUOTE = internAndExportCLSymbol("QUOTE");
	SymbolStruct RANDOM = internAndExportCLSymbol("RANDOM");
	SymbolStruct RANDOM_STATE = internAndExportCLSymbol("RANDOM-STATE");
	SymbolStruct RANDOM_STATE_P = internAndExportCLSymbol("RANDOM-STATE-P");
	SymbolStruct RASSOC = internAndExportCLSymbol("RASSOC");
	SymbolStruct RASSOC_IF = internAndExportCLSymbol("RASSOC-IF");
	SymbolStruct RASSOC_IF_NOT = internAndExportCLSymbol("RASSOC-IF-NOT");
	SymbolStruct RATIO = internAndExportCLSymbol("RATIO");
	SymbolStruct RATIONAL = internAndExportCLSymbol("RATIONAL");
	SymbolStruct RATIONALIZE = internAndExportCLSymbol("RATIONALIZE");
	SymbolStruct RATIONALP = internAndExportCLSymbol("RATIONALP");
	SymbolStruct READ = internAndExportCLSymbol("READ");
	SymbolStruct READ_BYTE = internAndExportCLSymbol("READ-BYTE");
	SymbolStruct READ_CHAR = internAndExportCLSymbol("READ-CHAR");
	SymbolStruct READ_CHAR_NO_HANG = internAndExportCLSymbol("READ-CHAR-NO-HANG");
	SymbolStruct READ_DELIMITED_LIST = internAndExportCLSymbol("READ-DELIMITED-LIST");
	SymbolStruct READ_FROM_STRING = internAndExportCLSymbol("READ-FROM-STRING");
	SymbolStruct READ_LINE = internAndExportCLSymbol("READ-LINE");
	SymbolStruct READ_PRESERVING_WHITESPACE = internAndExportCLSymbol("READ-PRESERVING-WHITESPACE");
	SymbolStruct READ_SEQUENCE = internAndExportCLSymbol("READ-SEQUENCE");
	SymbolStruct READER_ERROR = internAndExportCLSymbol("READER-ERROR");
	SymbolStruct READTABLE = internAndExportCLSymbol("READTABLE");
	SymbolStruct READTABLE_CASE = internAndExportCLSymbol("READTABLE-CASE");
	SymbolStruct READTABLEP = internAndExportCLSymbol("READTABLEP");
	SymbolStruct REAL = internAndExportCLSymbol("REAL");
	SymbolStruct REALP = internAndExportCLSymbol("REALP");
	SymbolStruct REALPART = internAndExportCLSymbol("REALPART");
	SymbolStruct REDUCE = internAndExportCLSymbol("REDUCE");
	SymbolStruct REINITIALIZE_INSTANCE = internAndExportCLSymbol("REINITIALIZE-INSTANCE");
	SymbolStruct REM = internAndExportCLSymbol("REM");
	SymbolStruct REMF = internAndExportCLSymbol("REMF");
	SymbolStruct REMHASH = internAndExportCLSymbol("REMHASH");
	SymbolStruct REMOVE = internAndExportCLSymbol("REMOVE");
	SymbolStruct REMOVE_DUPLICATES = internAndExportCLSymbol("REMOVE-DUPLICATES");
	SymbolStruct REMOVE_IF = internAndExportCLSymbol("REMOVE-IF");
	SymbolStruct REMOVE_IF_NOT = internAndExportCLSymbol("REMOVE-IF-NOT");
	SymbolStruct REMOVE_METHOD = internAndExportCLSymbol("REMOVE-METHOD");
	SymbolStruct REMPROP = internAndExportCLSymbol("REMPROP");
	SymbolStruct RENAME_FILE = internAndExportCLSymbol("RENAME-FILE");
	SymbolStruct RENAME_PACKAGE = internAndExportCLSymbol("RENAME-PACKAGE");
	SymbolStruct REPLACE = internAndExportCLSymbol("REPLACE");
	SymbolStruct REQUIRE = internAndExportCLSymbol("REQUIRE");
	SymbolStruct REST = internAndExportCLSymbol("REST");
	SymbolStruct RESTART = internAndExportCLSymbol("RESTART");
	SymbolStruct RESTART_BIND = internAndExportCLSymbol("RESTART-BIND");
	SymbolStruct RESTART_CASE = internAndExportCLSymbol("RESTART-CASE");
	SymbolStruct RESTART_NAME = internAndExportCLSymbol("RESTART-NAME");
	SymbolStruct RETURN = internAndExportCLSymbol("RETURN");
	SymbolStruct RETURN_FROM = internAndExportCLSymbol("RETURN-FROM");
	SymbolStruct REVAPPEND = internAndExportCLSymbol("REVAPPEND");
	SymbolStruct REVERSE = internAndExportCLSymbol("REVERSE");
	SymbolStruct ROOM = internAndExportCLSymbol("ROOM");
	SymbolStruct ROTATEF = internAndExportCLSymbol("ROTATEF");
	SymbolStruct ROUND = internAndExportCLSymbol("ROUND");
	SymbolStruct ROW_MAJOR_AREF = internAndExportCLSymbol("ROW-MAJOR-AREF");
	SymbolStruct RPLACA = internAndExportCLSymbol("RPLACA");
	SymbolStruct RPLACD = internAndExportCLSymbol("RPLACD");
	// SAFETY
	SymbolStruct SATISFIES = internAndExportCLSymbol("SATISFIES");
	SymbolStruct SBIT = internAndExportCLSymbol("SBIT");
	SymbolStruct SCALE_FLOAT = internAndExportCLSymbol("SCALE-FLOAT");
	SymbolStruct SCHAR = internAndExportCLSymbol("SCHAR");
	SymbolStruct SEARCH = internAndExportCLSymbol("SEARCH");
	SymbolStruct SECOND = internAndExportCLSymbol("SECOND");
	SymbolStruct SEQUENCE = internAndExportCLSymbol("SEQUENCE");
	SymbolStruct SERIOUS_CONDITION = internAndExportCLSymbol("SERIOUS-CONDITION");
	SymbolStruct SET = internAndExportCLSymbol("SET");
	SymbolStruct SET_DIFFERENCE = internAndExportCLSymbol("SET-DIFFERENCE");
	SymbolStruct SET_DISPATCH_MACRO_CHARACTER = internAndExportCLSymbol("SET-DISPATCH-MACRO-CHARACTER");
	SymbolStruct SET_EXCLUSIVE_OR = internAndExportCLSymbol("SET-EXCLUSIVE-OR");
	SymbolStruct SET_MACRO_CHARACTER = internAndExportCLSymbol("SET-MACRO-CHARACTER");
	SymbolStruct SET_PPRINT_DISPATCH = internAndExportCLSymbol("SET-PPRINT-DISPATCH");
	SymbolStruct SET_SYNTAX_FROM_CHAR = internAndExportCLSymbol("SET-SYNTAX-FROM-CHAR");
	SymbolStruct SETF = internAndExportCLSymbol("SETF");
	SymbolStruct SETQ = internAndExportCLSymbol("SETQ");
	SymbolStruct SEVENTH = internAndExportCLSymbol("SEVENTH");
	SymbolStruct SHADOW = internAndExportCLSymbol("SHADOW");
	SymbolStruct SHADOWING_IMPORT = internAndExportCLSymbol("SHADOWING-IMPORT");
	SymbolStruct SHARED_INITIALIZE = internAndExportCLSymbol("SHARED-INITIALIZE");
	SymbolStruct SHIFTF = internAndExportCLSymbol("SHIFTF");
	SymbolStruct SHORT_FLOAT = internAndExportCLSymbol("SHORT-FLOAT");
	// SHORT-FLOAT-EPSILON
	// SHORT-FLOAT-NEGATIVE-EPSILON
	SymbolStruct SHORT_SITE_NAME = internAndExportCLSymbol("SHORT-SITE-NAME");
	SymbolStruct SIGNAL = internAndExportCLSymbol("SIGNAL");
	SymbolStruct SIGNED_BYTE = internAndExportCLSymbol("SIGNED-BYTE");
	SymbolStruct SIGNUM = internAndExportCLSymbol("SIGNUM");
	SymbolStruct SIMPLE_ARRAY = internAndExportCLSymbol("SIMPLE-ARRAY");
	SymbolStruct SIMPLE_BASE_STRING = internAndExportCLSymbol("SIMPLE-BASE-STRING");
	SymbolStruct SIMPLE_BIT_VECTOR = internAndExportCLSymbol("SIMPLE-BIT-VECTOR");
	SymbolStruct SIMPLE_BIT_VECTOR_P = internAndExportCLSymbol("SIMPLE-BIT-VECTOR-P");
	SymbolStruct SIMPLE_CONDITION = internAndExportCLSymbol("SIMPLE-CONDITION");
	SymbolStruct SIMPLE_CONDITION_FORMAT_ARGUMENTS = internAndExportCLSymbol("SIMPLE-CONDITION-FORMAT-ARGUMENTS");
	SymbolStruct SIMPLE_CONDITION_FORMAT_CONTROL = internAndExportCLSymbol("SIMPLE-CONDITION-FORMAT-CONTROL");
	SymbolStruct SIMPLE_ERROR = internAndExportCLSymbol("SIMPLE-ERROR");
	SymbolStruct SIMPLE_STRING = internAndExportCLSymbol("SIMPLE-STRING");
	SymbolStruct SIMPLE_STRING_P = internAndExportCLSymbol("SIMPLE-STRING-P");
	SymbolStruct SIMPLE_TYPE_ERROR = internAndExportCLSymbol("SIMPLE-TYPE-ERROR");
	SymbolStruct SIMPLE_VECTOR = internAndExportCLSymbol("SIMPLE-VECTOR");
	SymbolStruct SIMPLE_VECTOR_P = internAndExportCLSymbol("SIMPLE-VECTOR-P");
	SymbolStruct SIMPLE_WARNING = internAndExportCLSymbol("SIMPLE-WARNING");
	SymbolStruct SIN = internAndExportCLSymbol("SIN");
	SymbolStruct SINGLE_FLOAT = internAndExportCLSymbol("SINGLE-FLOAT");
	// SINGLE-FLOAT-EPSILON
	// SINGLE-FLOAT-NEGATIVE-EPSILON
	SymbolStruct SINH = internAndExportCLSymbol("SINH");
	SymbolStruct SIXTH = internAndExportCLSymbol("SIXTH");
	SymbolStruct SLEEP = internAndExportCLSymbol("SLEEP");
	SymbolStruct SLOT_BOUNDP = internAndExportCLSymbol("SLOT-BOUNDP");
	SymbolStruct SLOT_EXISTS_P = internAndExportCLSymbol("SLOT-EXISTS-P");
	SymbolStruct SLOT_MAKUNBOUND = internAndExportCLSymbol("SLOT-MAKUNBOUND");
	SymbolStruct SLOT_MISSING = internAndExportCLSymbol("SLOT-MISSING");
	SymbolStruct SLOT_UNBOUND = internAndExportCLSymbol("SLOT-UNBOUND");
	SymbolStruct SLOT_VALUE = internAndExportCLSymbol("SLOT-VALUE");
	SymbolStruct SOFTWARE_TYPE = internAndExportCLSymbol("SOFTWARE-TYPE");
	SymbolStruct SOFTWARE_VERSION = internAndExportCLSymbol("SOFTWARE-VERSION");
	SymbolStruct SOME = internAndExportCLSymbol("SOME");
	SymbolStruct SORT = internAndExportCLSymbol("SORT");
	// SPACE
	// SPECIAL
	SymbolStruct SPECIAL_OPERATOR_P = internAndExportCLSymbol("SPECIAL-OPERATOR-P");
	// SPEED
	SymbolStruct SQRT = internAndExportCLSymbol("SQRT");
	SymbolStruct STABLE_SORT = internAndExportCLSymbol("STABLE-SORT");
	SymbolStruct STANDARD = internAndExportCLSymbol("STANDARD");
	SymbolStruct STANDARD_CHAR = internAndExportCLSymbol("STANDARD-CHAR");
	SymbolStruct STANDARD_CHAR_P = internAndExportCLSymbol("STANDARD-CHAR-P");
	SymbolStruct STANDARD_CLASS = internAndExportCLSymbol("STANDARD-CLASS");
	SymbolStruct STANDARD_GENERIC_FUNCTION = internAndExportCLSymbol("STANDARD-GENERIC-FUNCTION");
	SymbolStruct STANDARD_METHOD = internAndExportCLSymbol("STANDARD-METHOD");
	SymbolStruct STANDARD_OBJECT = internAndExportCLSymbol("STANDARD-OBJECT");
	SymbolStruct STEP = internAndExportCLSymbol("STEP");
	SymbolStruct STORAGE_CONDITION = internAndExportCLSymbol("STORAGE-CONDITION");
	SymbolStruct STORE_VALUE = internAndExportCLSymbol("STORE-VALUE");
	SymbolStruct STREAM = internAndExportCLSymbol("STREAM");
	SymbolStruct STREAM_ELEMENT_TYPE = internAndExportCLSymbol("STREAM-ELEMENT-TYPE");
	SymbolStruct STREAM_ERROR = internAndExportCLSymbol("STREAM-ERROR");
	SymbolStruct STREAM_ERROR_STREAM = internAndExportCLSymbol("STREAM-ERROR-STREAM");
	SymbolStruct STREAM_EXTERNAL_FORMAT = internAndExportCLSymbol("STREAM-EXTERNAL-FORMAT");
	SymbolStruct STREAMP = internAndExportCLSymbol("STREAMP");
	SymbolStruct STRING = internAndExportCLSymbol("STRING");
	SymbolStruct STRING_CAPITALIZE = internAndExportCLSymbol("STRING-CAPITALIZE");
	SymbolStruct STRING_DOWNCASE = internAndExportCLSymbol("STRING-DOWNCASE");
	SymbolStruct STRING_EQUAL = internAndExportCLSymbol("STRING-EQUAL");
	SymbolStruct STRING_GREATERP = internAndExportCLSymbol("STRING-GREATERP");
	SymbolStruct STRING_LEFT_TRIM = internAndExportCLSymbol("STRING-LEFT-TRIM");
	SymbolStruct STRING_LESSP = internAndExportCLSymbol("STRING-LESSP");
	SymbolStruct STRING_NOT_EQUAL = internAndExportCLSymbol("STRING-NOT-EQUAL");
	SymbolStruct STRING_NOT_GREATERP = internAndExportCLSymbol("STRING-NOT-GREATERP");
	SymbolStruct STRING_NOT_LESSP = internAndExportCLSymbol("STRING-NOT-LESSP");
	SymbolStruct STRING_RIGHT_TRIM = internAndExportCLSymbol("STRING-RIGHT-TRIM");
	SymbolStruct STRING_STREAM = internAndExportCLSymbol("STRING-STREAM");
	SymbolStruct STRING_TRIM = internAndExportCLSymbol("STRING-TRIM");
	SymbolStruct STRING_UPCASE = internAndExportCLSymbol("STRING-UPCASE");
	SymbolStruct STRING_NOT_EQUAL_CS = internAndExportCLSymbol("STRING/=");
	SymbolStruct STRING_LESSP_CS = internAndExportCLSymbol("STRING<");
	SymbolStruct STRING_NOT_GREATERP_CS = internAndExportCLSymbol("STRING<=");
	SymbolStruct STRING_EQUAL_CS = internAndExportCLSymbol("STRING=");
	SymbolStruct STRING_GREATERP_CS = internAndExportCLSymbol("STRING>");
	SymbolStruct STRING_NOT_LESSP_CS = internAndExportCLSymbol("STRING>=");
	SymbolStruct STRINGP = internAndExportCLSymbol("STRINGP");
	SymbolStruct STRUCTURE = internAndExportCLSymbol("STRUCTURE");
	SymbolStruct STRUCTURE_CLASS = internAndExportCLSymbol("STRUCTURE-CLASS");
	SymbolStruct STRUCTURE_OBJECT = internAndExportCLSymbol("STRUCTURE-OBJECT");
	SymbolStruct STYLE_WARNING = internAndExportCLSymbol("STYLE-WARNING");
	SymbolStruct SUBLIS = internAndExportCLSymbol("SUBLIS");
	SymbolStruct SUBSEQ = internAndExportCLSymbol("SUBSEQ");
	SymbolStruct SUBSETP = internAndExportCLSymbol("SUBSETP");
	SymbolStruct SUBST = internAndExportCLSymbol("SUBST");
	SymbolStruct SUBST_IF = internAndExportCLSymbol("SUBST-IF");
	SymbolStruct SUBST_IF_NOT = internAndExportCLSymbol("SUBST-IF-NOT");
	SymbolStruct SUBSTITUTE = internAndExportCLSymbol("SUBSTITUTE");
	SymbolStruct SUBSTITUTE_IF = internAndExportCLSymbol("SUBSTITUTE-IF");
	SymbolStruct SUBSTITUTE_IF_NOT = internAndExportCLSymbol("SUBSTITUTE-IF-NOT");
	SymbolStruct SUBTYPEP = internAndExportCLSymbol("SUBTYPEP");
	SymbolStruct SVREF = internAndExportCLSymbol("SVREF");
	SymbolStruct SXHASH = internAndExportCLSymbol("SXHASH");
	SymbolStruct SYMBOL = internAndExportCLSymbol("SYMBOL");
	SymbolStruct SYMBOL_FUNCTION = internAndExportCLSymbol("SYMBOL-FUNCTION");
	SymbolStruct SYMBOL_MACROLET = internAndExportCLSymbol("SYMBOL-MACROLET");
	SymbolStruct SYMBOL_NAME = internAndExportCLSymbol("SYMBOL-NAME");
	SymbolStruct SYMBOL_PACKAGE = internAndExportCLSymbol("SYMBOL-PACKAGE");
	SymbolStruct SYMBOL_PLIST = internAndExportCLSymbol("SYMBOL-PLIST");
	SymbolStruct SYMBOL_VALUE = internAndExportCLSymbol("SYMBOL-VALUE");
	SymbolStruct SYMBOLP = internAndExportCLSymbol("SYMBOLP");
	SymbolStruct SYNONYM_STREAM = internAndExportCLSymbol("SYNONYM-STREAM");
	SymbolStruct SYNONYM_STREAM_SYMBOL = internAndExportCLSymbol("SYNONYM-STREAM-SYMBOL");
	// T
	SymbolStruct TAGBODY = internAndExportCLSymbol("TAGBODY");
	SymbolStruct TAILP = internAndExportCLSymbol("TAILP");
	SymbolStruct TAN = internAndExportCLSymbol("TAN");
	SymbolStruct TANH = internAndExportCLSymbol("TANH");
	SymbolStruct TENTH = internAndExportCLSymbol("TENTH");
	SymbolStruct TERPRI = internAndExportCLSymbol("TERPRI");
	SymbolStruct THE = internAndExportCLSymbol("THE");
	SymbolStruct THIRD = internAndExportCLSymbol("THIRD");
	SymbolStruct THROW = internAndExportCLSymbol("THROW");
	SymbolStruct TIME = internAndExportCLSymbol("TIME");
	SymbolStruct TRACE = internAndExportCLSymbol("TRACE");
	SymbolStruct TRANSLATE_LOGICAL_PATHNAME = internAndExportCLSymbol("TRANSLATE-LOGICAL-PATHNAME");
	SymbolStruct TRANSLATE_PATHNAME = internAndExportCLSymbol("TRANSLATE-PATHNAME");
	SymbolStruct TREE_EQUAL = internAndExportCLSymbol("TREE-EQUAL");
	SymbolStruct TRUENAME = internAndExportCLSymbol("TRUENAME");
	SymbolStruct TRUNCATE = internAndExportCLSymbol("TRUNCATE");
	SymbolStruct TWO_WAY_STREAM = internAndExportCLSymbol("TWO-WAY-STREAM");
	SymbolStruct TWO_WAY_STREAM_INPUT_STREAM = internAndExportCLSymbol("TWO-WAY-STREAM-INPUT-STREAM");
	SymbolStruct TWO_WAY_STREAM_OUTPUT_STREAM = internAndExportCLSymbol("TWO-WAY-STREAM-OUTPUT-STREAM");
	// TYPE
	SymbolStruct TYPE_ERROR = internAndExportCLSymbol("TYPE-ERROR");
	SymbolStruct TYPE_ERROR_DATUM = internAndExportCLSymbol("TYPE-ERROR-DATUM");
	SymbolStruct TYPE_ERROR_EXPECTED_TYPE = internAndExportCLSymbol("TYPE-ERROR-EXPECTED-TYPE");
	SymbolStruct TYPE_OF = internAndExportCLSymbol("TYPE-OF");
	SymbolStruct TYPECASE = internAndExportCLSymbol("TYPECASE");
	SymbolStruct TYPEP = internAndExportCLSymbol("TYPEP");
	SymbolStruct UNBOUND_SLOT = internAndExportCLSymbol("UNBOUND-SLOT");
	SymbolStruct UNBOUND_SLOT_INSTANCE = internAndExportCLSymbol("UNBOUND-SLOT-INSTANCE");
	SymbolStruct UNBOUND_VARIABLE = internAndExportCLSymbol("UNBOUND-VARIABLE");
	SymbolStruct UNDEFINED_FUNCTION = internAndExportCLSymbol("UNDEFINED-FUNCTION");
	SymbolStruct UNEXPORT = internAndExportCLSymbol("UNEXPORT");
	SymbolStruct UNINTERN = internAndExportCLSymbol("UNINTERN");
	SymbolStruct UNION = internAndExportCLSymbol("UNION");
	SymbolStruct UNLESS = internAndExportCLSymbol("UNLESS");
	SymbolStruct UNREAD_CHAR = internAndExportCLSymbol("UNREAD-CHAR");
	SymbolStruct UNSIGNED_BYTE = internAndExportCLSymbol("UNSIGNED-BYTE");
	SymbolStruct UNTRACE = internAndExportCLSymbol("UNTRACE");
	SymbolStruct UNUSE_PACKAGE = internAndExportCLSymbol("UNUSE-PACKAGE");
	SymbolStruct UNWIND_PROTECT = internAndExportCLSymbol("UNWIND-PROTECT");
	SymbolStruct UPDATE_INSTANCE_FOR_DIFFERENT_CLASS = internAndExportCLSymbol("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS");
	SymbolStruct UPDATE_INSTANCE_FOR_REDEFINED_CLASS = internAndExportCLSymbol("UPDATE-INSTANCE-FOR-REDEFINED-CLASS");
	SymbolStruct UPGRADED_ARRAY_ELEMENT_TYPE = internAndExportCLSymbol("UPGRADED-ARRAY-ELEMENT-TYPE");
	SymbolStruct UPGRADED_COMPLEX_PART_TYPE = internAndExportCLSymbol("UPGRADED-COMPLEX-PART-TYPE");
	SymbolStruct UPPER_CASE_P = internAndExportCLSymbol("UPPER-CASE-P");
	SymbolStruct USE_PACKAGE = internAndExportCLSymbol("USE-PACKAGE");
	SymbolStruct USE_VALUE = internAndExportCLSymbol("USE-VALUE");
	SymbolStruct USER_HOMEDIR_PATHNAME = internAndExportCLSymbol("USER-HOMEDIR-PATHNAME");
	SymbolStruct VALUES = internAndExportCLSymbol("VALUES");
	SymbolStruct VALUES_LIST = internAndExportCLSymbol("VALUES-LIST");
	SymbolStruct VARIABLE = internAndExportCLSymbol("VARIABLE");
	SymbolStruct VECTOR = internAndExportCLSymbol("VECTOR");
	SymbolStruct VECTOR_POP = internAndExportCLSymbol("VECTOR-POP");
	SymbolStruct VECTOR_PUSH = internAndExportCLSymbol("VECTOR-PUSH");
	SymbolStruct VECTOR_PUSH_EXTEND = internAndExportCLSymbol("VECTOR-PUSH-EXTEND");
	SymbolStruct VECTORP = internAndExportCLSymbol("VECTORP");
	SymbolStruct WARN = internAndExportCLSymbol("WARN");
	SymbolStruct WARNING = internAndExportCLSymbol("WARNING");
	SymbolStruct WHEN = internAndExportCLSymbol("WHEN");
	SymbolStruct WILD_PATHNAME_P = internAndExportCLSymbol("WILD-PATHNAME-P");
	SymbolStruct WITH_ACCESSORS = internAndExportCLSymbol("WITH-ACCESSORS");
	SymbolStruct WITH_COMPILATION_UNIT = internAndExportCLSymbol("WITH-COMPILATION-UNIT");
	SymbolStruct WITH_CONDITION_RESTARTS = internAndExportCLSymbol("WITH-CONDITION-RESTARTS");
	SymbolStruct WITH_HASH_TABLE_ITERATOR = internAndExportCLSymbol("WITH-HASH-TABLE-ITERATOR");
	SymbolStruct WITH_INPUT_FROM_STRING = internAndExportCLSymbol("WITH-INPUT-FROM-STRING");
	SymbolStruct WITH_OPEN_FILE = internAndExportCLSymbol("WITH-OPEN-FILE");
	SymbolStruct WITH_OPEN_STREAM = internAndExportCLSymbol("WITH-OPEN-STREAM");
	SymbolStruct WITH_OUTPUT_TO_STRING = internAndExportCLSymbol("WITH-OUTPUT-TO-STRING");
	SymbolStruct WITH_PACKAGE_ITERATOR = internAndExportCLSymbol("WITH-PACKAGE-ITERATOR");
	SymbolStruct WITH_SIMPLE_RESTART = internAndExportCLSymbol("WITH-SIMPLE-RESTART");
	SymbolStruct WITH_SLOTS = internAndExportCLSymbol("WITH-SLOTS");
	SymbolStruct WITH_STANDARD_IO_SYNTAX = internAndExportCLSymbol("WITH-STANDARD-IO-SYNTAX");
	SymbolStruct WRITE = internAndExportCLSymbol("WRITE");
	SymbolStruct WRITE_BYTE = internAndExportCLSymbol("WRITE-BYTE");
	SymbolStruct WRITE_CHAR = internAndExportCLSymbol("WRITE-CHAR");
	SymbolStruct WRITE_LINE = internAndExportCLSymbol("WRITE-LINE");
	SymbolStruct WRITE_SEQUENCE = internAndExportCLSymbol("WRITE-SEQUENCE");
	SymbolStruct WRITE_STRING = internAndExportCLSymbol("WRITE-STRING");
	SymbolStruct WRITE_TO_STRING = internAndExportCLSymbol("WRITE-TO-STRING");
	SymbolStruct Y_OR_N_P = internAndExportCLSymbol("Y-OR-N-P");
	SymbolStruct YES_OR_NO_P = internAndExportCLSymbol("YES-OR-NO-P");
	SymbolStruct ZEROP = internAndExportCLSymbol("ZEROP");

	// COMPILER
	SymbolStruct DEFSTRUCT_SO = internAndExportCompilerSymbol("%DEFSTRUCT");
	SymbolStruct MACRO_LAMBDA = internAndExportCompilerSymbol("MACRO-LAMBDA");
	SymbolStruct TAIL_CALL = internAndExportCompilerSymbol("%TAIL-CALL");
	SymbolStruct TAIL_RECURSION = internAndExportCompilerSymbol("%TAIL-RECURSION");

	// System
	SymbolStruct SYSTEM_IN_PACKAGE = internAndExportSystemSymbol("%IN-PACKAGE");
	SymbolStruct SET_SYMBOL_FUNCTION = internAndExportSystemSymbol("SET-SYMBOL-FUNCTION");
	SymbolStruct SET_SYMBOL_MACRO = internAndExportSystemSymbol("SET-SYMBOL-MACRO");
	SymbolStruct SET_SYMBOL_SETF_FUNCTION = internAndExportSystemSymbol("SET-SYMBOL-SETF-FUNCTION");
	SymbolStruct LIST_TO_VECTOR = internAndExportSystemSymbol("LIST-TO-VECTOR");
	SymbolStruct READ_DISPATCH_CHARACTER = internAndExportSystemSymbol("READ-DISPATCH-CHARACTER");
	SymbolStruct BIND_SYMBOL_FUNCTION = internAndExportSystemSymbol("BIND-SYMBOL-FUNCTION");
	SymbolStruct UNBIND_SYMBOL_FUNCTION = internAndExportSystemSymbol("UNBIND-SYMBOL-FUNCTION");

	SymbolStruct JAVA_CLASS_NAME = internAndExportSystemSymbol("%JAVA-CLASS-NAME");
	SymbolStruct LISP_NAME = internAndExportSystemSymbol("%LISP-NAME");

	SymbolStruct MACRO_FUNCTION_DEFINITION = GlobalPackageStruct.SYSTEM.intern("MACRO-FUNCTION-DEFINITION").getSymbol();
	SymbolStruct SYMBOL_MACRO_DEFINITION = GlobalPackageStruct.SYSTEM.intern("SYMBOL-MACRO-DEFINITION").getSymbol();
	SymbolStruct SETF_DEFINITION = GlobalPackageStruct.SYSTEM.intern("SETF-DEFINITION").getSymbol();

	// Backquote
	SymbolStruct BQ_COMMA_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",").getSymbol();
	SymbolStruct BQ_AT_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",@").getSymbol();
	SymbolStruct BQ_DOT_FLAG = GlobalPackageStruct.BACKQUOTE.intern(",.").getSymbol();
	SymbolStruct BQ_VECTOR_FLAG = GlobalPackageStruct.BACKQUOTE.intern("bqv").getSymbol();

	// Standard Keywords
	KeywordStruct ELEMENT_TYPE_KEYWORD = KeywordStruct.toLispKeyword("ELEMENT-TYPE");
	KeywordStruct INITIAL_ELEMENT_KEYWORD = KeywordStruct.toLispKeyword("INITIAL-ELEMENT");
	KeywordStruct INITIAL_CONTENTS_KEYWORD = KeywordStruct.toLispKeyword("INITIAL-CONTENTS");

	// Package Keywords
	KeywordStruct INTERNAL_KEYWORD = KeywordStruct.toLispKeyword("INTERNAL");
	KeywordStruct EXTERNAL_KEYWORD = KeywordStruct.toLispKeyword("EXTERNAL");
	KeywordStruct INHERITED_KEYWORD = KeywordStruct.toLispKeyword("INHERITED");

	// Stream Keywords
	KeywordStruct DEFAULT_KEYWORD = KeywordStruct.toLispKeyword("DEFAULT");
	KeywordStruct ABORT_KEYWORD = KeywordStruct.toLispKeyword("ABORT");
	KeywordStruct START_KEYWORD = KeywordStruct.toLispKeyword("START");
	KeywordStruct END_KEYWORD = KeywordStruct.toLispKeyword("END");
	KeywordStruct DIRECTION_KEYWORD = KeywordStruct.toLispKeyword("DIRECTION");
	KeywordStruct INPUT_KEYWORD = KeywordStruct.toLispKeyword("INPUT");
	KeywordStruct OUTPUT_KEYWORD = KeywordStruct.toLispKeyword("OUTPUT");
	KeywordStruct IO_KEYWORD = KeywordStruct.toLispKeyword("IO");
	KeywordStruct PROBE_KEYWORD = KeywordStruct.toLispKeyword("PROBE");
	KeywordStruct IF_EXISTS_KEYWORD = KeywordStruct.toLispKeyword("IF-EXISTS");
	KeywordStruct ERROR_KEYWORD = KeywordStruct.toLispKeyword("ERROR");
	KeywordStruct NEW_VERSION_KEYWORD = KeywordStruct.toLispKeyword("NEW-VERSION");
	KeywordStruct RENAME_KEYWORD = KeywordStruct.toLispKeyword("RENAME");
	KeywordStruct RENAME_AND_DELETE_KEYWORD = KeywordStruct.toLispKeyword("RENAME-AND-DELETE");
	KeywordStruct OVERWRITE_KEYWORD = KeywordStruct.toLispKeyword("OVERWRITE");
	KeywordStruct APPEND_KEYWORD = KeywordStruct.toLispKeyword("APPEND");
	KeywordStruct SUPERSEDE_KEYWORD = KeywordStruct.toLispKeyword("SUPERSEDE");
	KeywordStruct CREATE_KEYWORD = KeywordStruct.toLispKeyword("CREATE");

	// Readtable Keywords
//	KeywordStruct UPCASE_KEYWORD = KeywordStruct.toLispKeyword("UPCASE");
//	KeywordStruct DOWNCASE_KEYWORD = KeywordStruct.toLispKeyword("DOWNCASE");
	KeywordStruct PRESERVE_KEYWORD = KeywordStruct.toLispKeyword("PRESERVE");
	KeywordStruct INVERT_KEYWORD = KeywordStruct.toLispKeyword("INVERT");

	// Features Keywords
	/**
	 * NOT {@link KeywordStruct} for processing features that should 'not' be included.
	 */
	KeywordStruct NOT_KEYWORD = KeywordStruct.toLispKeyword("NOT");

	/**
	 * AND {@link KeywordStruct} for processing features that should be included via 'and' operation.
	 */
	KeywordStruct AND_KEYWORD = KeywordStruct.toLispKeyword("AND");

	/**
	 * OR {@link KeywordStruct} for processing features that should be included via 'or' operation.
	 */
	KeywordStruct OR_KEYWORD = KeywordStruct.toLispKeyword("OR");

	// Eval-When Keywords
	KeywordStruct COMPILE_TOPLEVEL = KeywordStruct.toLispKeyword("COMPILE-TOPLEVEL");
	KeywordStruct LOAD_TOPLEVEL = KeywordStruct.toLispKeyword("LOAD-TOPLEVEL");
	KeywordStruct EXECUTE = KeywordStruct.toLispKeyword("EXECUTE");

	// Print-Case Keywords
	KeywordStruct UPCASE_KEYWORD = KeywordStruct.toLispKeyword("UPCASE");
	KeywordStruct DOWNCASE_KEYWORD = KeywordStruct.toLispKeyword("DOWNCASE");
	KeywordStruct CAPITALIZE_KEYWORD = KeywordStruct.toLispKeyword("CAPITALIZE");

	// Pathname Keywords
	KeywordStruct WILD_KEYWORD = KeywordStruct.toLispKeyword("WILD");
	KeywordStruct WILD_INFERIORS_KEYWORD = KeywordStruct.toLispKeyword("WILD-INFERIORS");
	KeywordStruct UNSPECIFIC_KEYWORD = KeywordStruct.toLispKeyword("UNSPECIFIC");
	KeywordStruct COMMON_KEYWORD = KeywordStruct.toLispKeyword("COMMON");
	KeywordStruct LOCAL_KEYWORD = KeywordStruct.toLispKeyword("LOCAL");
	KeywordStruct RELATIVE_KEYWORD = KeywordStruct.toLispKeyword("RELATIVE");
	KeywordStruct ABSOLUTE_KEYWORD = KeywordStruct.toLispKeyword("ABSOLUTE");
	KeywordStruct BACK_KEYWORD = KeywordStruct.toLispKeyword("BACK");
	KeywordStruct UP_KEYWORD = KeywordStruct.toLispKeyword("UP");
	KeywordStruct NEWEST_KEYWORD = KeywordStruct.toLispKeyword("NEWEST");
	KeywordStruct OLDEST_KEYWORD = KeywordStruct.toLispKeyword("OLDEST");
	KeywordStruct CASE_KEYWORD = KeywordStruct.toLispKeyword("CASE");
	KeywordStruct HOST_KEYWORD = KeywordStruct.toLispKeyword("HOST");
	KeywordStruct DEVICE_KEYWORD = KeywordStruct.toLispKeyword("DEVICE");
	KeywordStruct DIRECTORY_KEYWORD = KeywordStruct.toLispKeyword("DIRECTORY");
	KeywordStruct NAME_KEYWORD = KeywordStruct.toLispKeyword("NAME");
	KeywordStruct TYPE_KEYWORD = KeywordStruct.toLispKeyword("TYPE");
	KeywordStruct VERSION_KEYWORD = KeywordStruct.toLispKeyword("VERSION");

	// Pathname URL Keywords
	KeywordStruct SCHEME_KEYWORD = KeywordStruct.toLispKeyword("SCHEME");
	KeywordStruct AUTHORITY_KEYWORD = KeywordStruct.toLispKeyword("AUTHORITY");
	KeywordStruct QUERY_KEYWORD = KeywordStruct.toLispKeyword("QUERY");
	KeywordStruct FRAGMENT_KEYWORD = KeywordStruct.toLispKeyword("FRAGMENT");

	// Compile-File/Load Keywords
	KeywordStruct OUTPUT_FILE_KEYWORD = KeywordStruct.toLispKeyword("OUTPUT-FILE");
	KeywordStruct VERBOSE_KEYWORD = KeywordStruct.toLispKeyword("VERBOSE");
	KeywordStruct PRINT_KEYWORD = KeywordStruct.toLispKeyword("PRINT");
	KeywordStruct IF_DOES_NOT_EXIST_KEYWORD = KeywordStruct.toLispKeyword("IF-DOES-NOT-EXIST");
	KeywordStruct EXTERNAL_FORMAT_KEYWORD = KeywordStruct.toLispKeyword("EXTERNAL-FORMAT");

	// Make-Package Keywords
	KeywordStruct NICKNAMES_KEYWORD = KeywordStruct.toLispKeyword("NICKNAMES");
	KeywordStruct USE_KEYWORD = KeywordStruct.toLispKeyword("USE");

	// Lambda List Keywords
	KeywordStruct ALLOW_OTHER_KEYS = KeywordStruct.toLispKeyword("ALLOW-OTHER-KEYS");

	// Extension
	SymbolStruct JCLASS = internAndExportExtensionSymbol("JCLASS");
	SymbolStruct JINVOKE = internAndExportExtensionSymbol("JINVOKE");
	SymbolStruct JINVOKE_STATIC = internAndExportExtensionSymbol("JINVOKE-STATIC");
	SymbolStruct JMETHOD = internAndExportExtensionSymbol("JMETHOD");
	SymbolStruct JNEW = internAndExportExtensionSymbol("JNEW");

	SymbolStruct NIL_VECTOR = internAndExportExtensionSymbol("NIL-VECTOR");

	SymbolStruct WHITESPACEP = internAndExportExtensionSymbol("WHITESPACEP");

	SymbolStruct STRING_INPUT_STREAM = internAndExportExtensionSymbol("STRING-INPUT-STREAM");
	SymbolStruct STRING_OUTPUT_STREAM = internAndExportExtensionSymbol("STRING-OUTPUT-STREAM");

	SymbolStruct FREE_MEMORY = internAndExportExtensionSymbol("FREE-MEMORY");
	SymbolStruct GC = internAndExportExtensionSymbol("GC");
	SymbolStruct HELP = internAndExportExtensionSymbol("HELP");
	SymbolStruct MAX_MEMORY = internAndExportExtensionSymbol("MAX-MEMORY");
	SymbolStruct TOTAL_MEMORY = internAndExportExtensionSymbol("TOTAL-MEMORY");

	SymbolStruct QUIT = internAndExportExtensionSymbol("QUIT");

	private static <S extends SymbolStruct> S importAndExportCLSymbol(final S symbol) {
		GlobalPackageStruct.COMMON_LISP.importSymbol(symbol);
		symbol.setSymbolPackage(GlobalPackageStruct.COMMON_LISP);

		GlobalPackageStruct.COMMON_LISP.export(symbol);
		return symbol;
	}

	private static SymbolStruct internAndExportCLSymbol(final String name) {
		final SymbolStruct symbol = GlobalPackageStruct.COMMON_LISP.intern(name).getSymbol();
		GlobalPackageStruct.COMMON_LISP.export(symbol);
		return symbol;
	}

	private static SymbolStruct internAndExportCompilerSymbol(final String name) {
		final SymbolStruct symbol = GlobalPackageStruct.COMPILER.intern(name).getSymbol();
		GlobalPackageStruct.COMPILER.export(symbol);
		return symbol;
	}

	private static SymbolStruct internAndExportSystemSymbol(final String name) {
		final SymbolStruct symbol = GlobalPackageStruct.SYSTEM.intern(name).getSymbol();
		GlobalPackageStruct.SYSTEM.export(symbol);
		return symbol;
	}

	private static SymbolStruct internAndExportExtensionSymbol(final String name) {
		final SymbolStruct symbol = GlobalPackageStruct.EXTENSIONS.intern(name).getSymbol();
		GlobalPackageStruct.EXTENSIONS.export(symbol);
		return symbol;
	}

	private static <TYPE extends LispStruct> ConstantStructImpl<TYPE> internAndExportCLConstant(final String name) {
		final ConstantStructImpl<TYPE> struct = ConstantStructImpl.valueOf(name, GlobalPackageStruct.COMMON_LISP);
		GlobalPackageStruct.COMMON_LISP.export(struct);
		return struct;
	}

	private static <TYPE extends LispStruct> VariableStructImpl<TYPE> internAndExportCLVariable(final String name) {
		final VariableStructImpl<TYPE> struct = VariableStructImpl.valueOf(name, GlobalPackageStruct.COMMON_LISP);
		GlobalPackageStruct.COMMON_LISP.export(struct);
		return struct;
	}
}
