package jcl.structs.symbols;

import jcl.LispStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Constant<T extends LispStruct> extends SymbolStruct<T> {

	public static final Constant<IntegerStruct> CALL_ARGUMENTS_LIMIT = new Constant<>("CALL-ARGUMENTS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	// TODO: what is the value for this symbol??
	public static final Constant<?> ALLOW_OTHER_KEYS = new Constant<>("&ALLOW-OTHER-KEYS", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> AUX = new Constant<>("&AUX", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> BODY = new Constant<>("&BODY", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> ENVIRONMENT = new Constant<>("&ENVIRONMENT", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> KEY = new Constant<>("&KEY", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> OPTIONAL = new Constant<>("&OPTIONAL", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> REST = new Constant<>("&REST", GlobalPackageStruct.COMMON_LISP, null);
	public static final Constant<?> WHOLE = new Constant<>("&WHOLE", GlobalPackageStruct.COMMON_LISP, null);

	public static final Constant<?> LAMBDA_LIST_KEYWORDS = new Constant<>("LAMBDA-LIST-KEYWORDS", GlobalPackageStruct.COMMON_LISP,
			ListStruct.buildProperList(
					ALLOW_OTHER_KEYS,
					AUX,
					BODY,
					ENVIRONMENT,
					KEY,
					OPTIONAL,
					REST,
					WHOLE));
	public static final Constant<IntegerStruct> LAMBDA_PARAMETERS_LIMIT = new Constant<>("LAMBDA-PARAMETERS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));
	public static final Constant<NILStruct> NIL = new Constant<>("NIL", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	public static final Constant<TStruct> T = new Constant<>("T", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	public static final Constant<IntegerStruct> MULTIPLE_VALUES_LIMIT = new Constant<>("MULTIPLE-VALUES-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));
	public static final Constant<FloatStruct> PI = new Constant<>("PI", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Math.PI)));

	public static final Constant<IntegerStruct> BOOLE_1 = new Constant<>("BOOLE-1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ZERO));
	public static final Constant<IntegerStruct> BOOLE_2 = new Constant<>("BOOLE-2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));
	public static final Constant<IntegerStruct> BOOLE_AND = new Constant<>("BOOLE-AND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(2)));
	public static final Constant<IntegerStruct> BOOLE_ANDC1 = new Constant<>("BOOLE-ANDC1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(3)));
	public static final Constant<IntegerStruct> BOOLE_ANDC2 = new Constant<>("BOOLE-ANDC2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(4)));
	public static final Constant<IntegerStruct> BOOLE_C1 = new Constant<>("BOOLE-C1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(5)));
	public static final Constant<IntegerStruct> BOOLE_C2 = new Constant<>("BOOLE-C2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(6)));
	public static final Constant<IntegerStruct> BOOLE_CLR = new Constant<>("BOOLE-CLR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(7)));
	public static final Constant<IntegerStruct> BOOLE_EQV = new Constant<>("BOOLE-EQV", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(8)));
	public static final Constant<IntegerStruct> BOOLE_IOR = new Constant<>("BOOLE-IOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(9)));
	public static final Constant<IntegerStruct> BOOLE_NAND = new Constant<>("BOOLE-NAND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.TEN));
	private static final int ELEVEN = 11;
	public static final Constant<IntegerStruct> BOOLE_NOR = new Constant<>("BOOLE-NOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(ELEVEN)));
	private static final int TWELVE = 12;
	public static final Constant<IntegerStruct> BOOLE_ORC1 = new Constant<>("BOOLE-ORC1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(TWELVE)));
	private static final int THIRTEEN = 13;
	public static final Constant<IntegerStruct> BOOLE_ORC2 = new Constant<>("BOOLE-ORC2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(THIRTEEN)));
	private static final int FOURTEEN = 14;
	public static final Constant<IntegerStruct> BOOLE_SET = new Constant<>("BOOLE-SET", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(FOURTEEN)));
	private static final int FIFTEEN = 15;
	public static final Constant<IntegerStruct> BOOLE_XOR = new Constant<>("BOOLE-XOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(FIFTEEN)));

	public static final Constant<IntegerStruct> MOST_POSITIVE_FIXNUM = new Constant<>("MOST-POSITIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));
	public static final Constant<IntegerStruct> MOST_NEGATIVE_FIXNUM = new Constant<>("MOST-NEGATIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MIN_VALUE)));

	public static final Constant<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = new Constant<>("MOST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MAX_VALUE)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = new Constant<>("LEAST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_NORMAL)));
	public static final Constant<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = new Constant<>("MOST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_VALUE)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = new Constant<>("LEAST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Float.MIN_NORMAL)));
	public static final Constant<FloatStruct> SHORT_FLOAT_EPSILON = new Constant<>("SHORT-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = new Constant<>("SHORT-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

	public static final Constant<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = new Constant<>("MOST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MAX_VALUE)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = new Constant<>("LEAST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_NORMAL)));
	public static final Constant<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = new Constant<>("MOST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_VALUE)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = new Constant<>("LEAST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Float.MIN_NORMAL)));
	public static final Constant<FloatStruct> SINGLE_FLOAT_EPSILON = new Constant<>("SINGLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = new Constant<>("SINGLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

	public static final Constant<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = new Constant<>("MOST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = new Constant<>("LEAST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	public static final Constant<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = new Constant<>("MOST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = new Constant<>("LEAST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	public static final Constant<FloatStruct> DOUBLE_FLOAT_EPSILON = new Constant<>("DOUBLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = new Constant<>("DOUBLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

	public static final Constant<FloatStruct> MOST_POSITIVE_LONG_FLOAT = new Constant<>("MOST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = new Constant<>("LEAST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	public static final Constant<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = new Constant<>("MOST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = new Constant<>("LEAST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	public static final Constant<FloatStruct> LONG_FLOAT_EPSILON = new Constant<>("LONG-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
	public static final Constant<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = new Constant<>("LONG-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

	public static final Constant<IntegerStruct> CHAR_CODE_LIMIT = new Constant<>("CHAR-CODE-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Character.MAX_VALUE)));

	public static final Constant<IntegerStruct> ARRAY_DIMENSION_LIMIT = new Constant<>("ARRAY-DIMENSION-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));
	public static final Constant<IntegerStruct> ARRAY_RANK_LIMIT = new Constant<>("ARRAY-RANK-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));
	public static final Constant<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = new Constant<>("ARRAY-TOTAL-SIZE-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));

	private static final int ONE_MILLION = 1000000;
	public static final Constant<IntegerStruct> INTERNAL_TIME_UNITS_PER_SECOND = new Constant<>("INTERNAL-TIME-UNITS-PER-SECOND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(ONE_MILLION)));

	public Constant(final String name, final PackageStruct symbolPackage, final T value) {
		super(name, symbolPackage, value);
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		throw new RuntimeException("Can't set package for constant " + name + '.');
	}

	@Override
	public void setValue(final T value) {
		throw new RuntimeException("Can't set value for constant " + name + '.');
	}

	@Override
	public void setFunction(final FunctionStruct function) {
		throw new RuntimeException("Can't set function for constant " + name + '.');
	}

	@Override
	public void setProperty(final LispStruct key, final LispStruct value) {
		throw new RuntimeException("Can't set properties for constant " + name + '.');
	}
}
