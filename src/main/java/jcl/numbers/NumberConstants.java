/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.ConstantStruct;

public interface NumberConstants {

	ConstantStruct<IntegerStruct> BOOLE_1 = new ConstantStruct<>("BOOLE-1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
	ConstantStruct<IntegerStruct> BOOLE_2 = new ConstantStruct<>("BOOLE-2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ONE);
	ConstantStruct<IntegerStruct> BOOLE_AND = new ConstantStruct<>("BOOLE-AND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.TWO);
	ConstantStruct<IntegerStruct> BOOLE_ANDC1 = new ConstantStruct<>("BOOLE-ANDC1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(3)));
	ConstantStruct<IntegerStruct> BOOLE_ANDC2 = new ConstantStruct<>("BOOLE-ANDC2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(4)));
	ConstantStruct<IntegerStruct> BOOLE_C1 = new ConstantStruct<>("BOOLE-C1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(5)));
	ConstantStruct<IntegerStruct> BOOLE_C2 = new ConstantStruct<>("BOOLE-C2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(6)));
	ConstantStruct<IntegerStruct> BOOLE_CLR = new ConstantStruct<>("BOOLE-CLR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(7)));
	ConstantStruct<IntegerStruct> BOOLE_EQV = new ConstantStruct<>("BOOLE-EQV", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(8)));
	ConstantStruct<IntegerStruct> BOOLE_IOR = new ConstantStruct<>("BOOLE-IOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(9)));
	ConstantStruct<IntegerStruct> BOOLE_NAND = new ConstantStruct<>("BOOLE-NAND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.TEN));
	ConstantStruct<IntegerStruct> BOOLE_NOR = new ConstantStruct<>("BOOLE-NOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(11)));
	ConstantStruct<IntegerStruct> BOOLE_ORC1 = new ConstantStruct<>("BOOLE-ORC1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(12)));
	ConstantStruct<IntegerStruct> BOOLE_ORC2 = new ConstantStruct<>("BOOLE-ORC2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(13)));
	ConstantStruct<IntegerStruct> BOOLE_SET = new ConstantStruct<>("BOOLE-SET", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(14)));
	ConstantStruct<IntegerStruct> BOOLE_XOR = new ConstantStruct<>("BOOLE-XOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(15)));

	ConstantStruct<IntegerStruct> MOST_POSITIVE_FIXNUM = new ConstantStruct<>("MOST-POSITIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
	ConstantStruct<IntegerStruct> MOST_NEGATIVE_FIXNUM = new ConstantStruct<>("MOST-NEGATIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MIN_VALUE)));

	ConstantStruct<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = new ConstantStruct<>("MOST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = new ConstantStruct<>("MOST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> SHORT_FLOAT_EPSILON = new ConstantStruct<>("SHORT-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(1.1102230246251568E-16)));
	ConstantStruct<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = new ConstantStruct<>("SHORT-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(5.551115123125784E-17)));

	ConstantStruct<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = new ConstantStruct<>("MOST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = new ConstantStruct<>("MOST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> SINGLE_FLOAT_EPSILON = new ConstantStruct<>("SINGLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(1.1102230246251568E-16)));
	ConstantStruct<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = new ConstantStruct<>("SINGLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(5.551115123125784E-17)));

	ConstantStruct<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = new ConstantStruct<>("MOST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = new ConstantStruct<>("MOST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> DOUBLE_FLOAT_EPSILON = new ConstantStruct<>("DOUBLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(1.1102230246251568E-16)));
	ConstantStruct<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = new ConstantStruct<>("DOUBLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(5.551115123125784E-17)));

	ConstantStruct<FloatStruct> MOST_POSITIVE_LONG_FLOAT = new ConstantStruct<>("MOST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = new ConstantStruct<>("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = new ConstantStruct<>("MOST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MAX_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_VALUE)));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = new ConstantStruct<>("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
	ConstantStruct<FloatStruct> LONG_FLOAT_EPSILON = new ConstantStruct<>("LONG-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(1.1102230246251568E-16)));
	ConstantStruct<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = new ConstantStruct<>("LONG-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(5.551115123125784E-17)));

	ConstantStruct<FloatStruct> PI = new ConstantStruct<>("PI", GlobalPackageStruct.COMMON_LISP, new SingleFloatStruct(BigDecimal.valueOf(Math.PI)));
}
