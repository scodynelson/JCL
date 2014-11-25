/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Constant;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface NumberConstants {

    Constant<IntegerStruct> BOOLE_1 = new Constant<>("BOOLE-1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ZERO));
    Constant<IntegerStruct> BOOLE_2 = new Constant<>("BOOLE-2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ONE));
    Constant<IntegerStruct> BOOLE_AND = new Constant<>("BOOLE-AND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(2)));
    Constant<IntegerStruct> BOOLE_ANDC1 = new Constant<>("BOOLE-ANDC1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(3)));
    Constant<IntegerStruct> BOOLE_ANDC2 = new Constant<>("BOOLE-ANDC2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(4)));
    Constant<IntegerStruct> BOOLE_C1 = new Constant<>("BOOLE-C1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(5)));
    Constant<IntegerStruct> BOOLE_C2 = new Constant<>("BOOLE-C2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(6)));
    Constant<IntegerStruct> BOOLE_CLR = new Constant<>("BOOLE-CLR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(7)));
    Constant<IntegerStruct> BOOLE_EQV = new Constant<>("BOOLE-EQV", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(8)));
    Constant<IntegerStruct> BOOLE_IOR = new Constant<>("BOOLE-IOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(9)));
    Constant<IntegerStruct> BOOLE_NAND = new Constant<>("BOOLE-NAND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.TEN));
    Constant<IntegerStruct> BOOLE_NOR = new Constant<>("BOOLE-NOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(11)));
    Constant<IntegerStruct> BOOLE_ORC1 = new Constant<>("BOOLE-ORC1", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(12)));
    Constant<IntegerStruct> BOOLE_ORC2 = new Constant<>("BOOLE-ORC2", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(13)));
    Constant<IntegerStruct> BOOLE_SET = new Constant<>("BOOLE-SET", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(14)));
    Constant<IntegerStruct> BOOLE_XOR = new Constant<>("BOOLE-XOR", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(15)));

    Constant<IntegerStruct> MOST_POSITIVE_FIXNUM = new Constant<>("MOST-POSITIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));
    Constant<IntegerStruct> MOST_NEGATIVE_FIXNUM = new Constant<>("MOST-NEGATIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MIN_VALUE)));

    Constant<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = new Constant<>("MOST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MAX_VALUE)));
    Constant<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = new Constant<>("LEAST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_NORMAL)));
    Constant<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = new Constant<>("MOST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_VALUE)));
    Constant<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = new Constant<>("LEAST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Float.MIN_NORMAL)));
    Constant<FloatStruct> SHORT_FLOAT_EPSILON = new Constant<>("SHORT-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = new Constant<>("SHORT-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

    Constant<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = new Constant<>("MOST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MAX_VALUE)));
    Constant<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = new Constant<>("LEAST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_NORMAL)));
    Constant<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = new Constant<>("MOST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Float.MIN_VALUE)));
    Constant<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = new Constant<>("LEAST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Float.MIN_NORMAL)));
    Constant<FloatStruct> SINGLE_FLOAT_EPSILON = new Constant<>("SINGLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = new Constant<>("SINGLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

    Constant<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = new Constant<>("MOST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
    Constant<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = new Constant<>("LEAST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
    Constant<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = new Constant<>("MOST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
    Constant<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = new Constant<>("LEAST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
    Constant<FloatStruct> DOUBLE_FLOAT_EPSILON = new Constant<>("DOUBLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = new Constant<>("DOUBLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

    Constant<FloatStruct> MOST_POSITIVE_LONG_FLOAT = new Constant<>("MOST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MAX_VALUE)));
    Constant<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = new Constant<>("LEAST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = new Constant<>("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_NORMAL)));
    Constant<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = new Constant<>("MOST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Double.MIN_VALUE)));
    Constant<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = new Constant<>("LEAST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = new Constant<>("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(-Double.MIN_NORMAL)));
    Constant<FloatStruct> LONG_FLOAT_EPSILON = new Constant<>("LONG-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));
    Constant<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = new Constant<>("LONG-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(0)));

    Constant<FloatStruct> PI = new Constant<>("PI", GlobalPackageStruct.COMMON_LISP, new FloatStruct(BigDecimal.valueOf(Math.PI)));
}
