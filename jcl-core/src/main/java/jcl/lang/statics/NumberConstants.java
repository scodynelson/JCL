/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.DoubleFloatStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.internal.ConstantStructImpl;
import org.apache.commons.math3.util.Precision;

public interface NumberConstants {

	ConstantStructImpl<IntegerStruct> BOOLE_1 = ConstantStructImpl.valueOf("BOOLE-1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
	ConstantStructImpl<IntegerStruct> BOOLE_2 = ConstantStructImpl.valueOf("BOOLE-2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ONE);
	ConstantStructImpl<IntegerStruct> BOOLE_AND = ConstantStructImpl.valueOf("BOOLE-AND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.TWO);
	ConstantStructImpl<IntegerStruct> BOOLE_ANDC1 = ConstantStructImpl.valueOf("BOOLE-ANDC1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(3));
	ConstantStructImpl<IntegerStruct> BOOLE_ANDC2 = ConstantStructImpl.valueOf("BOOLE-ANDC2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(4));
	ConstantStructImpl<IntegerStruct> BOOLE_C1 = ConstantStructImpl.valueOf("BOOLE-C1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(5));
	ConstantStructImpl<IntegerStruct> BOOLE_C2 = ConstantStructImpl.valueOf("BOOLE-C2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(6));
	ConstantStructImpl<IntegerStruct> BOOLE_CLR = ConstantStructImpl.valueOf("BOOLE-CLR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(7));
	ConstantStructImpl<IntegerStruct> BOOLE_EQV = ConstantStructImpl.valueOf("BOOLE-EQV", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(8));
	ConstantStructImpl<IntegerStruct> BOOLE_IOR = ConstantStructImpl.valueOf("BOOLE-IOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(9));
	ConstantStructImpl<IntegerStruct> BOOLE_NAND = ConstantStructImpl.valueOf("BOOLE-NAND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(10));
	ConstantStructImpl<IntegerStruct> BOOLE_NOR = ConstantStructImpl.valueOf("BOOLE-NOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(11));
	ConstantStructImpl<IntegerStruct> BOOLE_ORC1 = ConstantStructImpl.valueOf("BOOLE-ORC1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(12));
	ConstantStructImpl<IntegerStruct> BOOLE_ORC2 = ConstantStructImpl.valueOf("BOOLE-ORC2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(13));
	ConstantStructImpl<IntegerStruct> BOOLE_SET = ConstantStructImpl.valueOf("BOOLE-SET", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(14));
	ConstantStructImpl<IntegerStruct> BOOLE_XOR = ConstantStructImpl.valueOf("BOOLE-XOR", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(15));

	ConstantStructImpl<IntegerStruct> MOST_POSITIVE_FIXNUM = ConstantStructImpl.valueOf("MOST-POSITIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Integer.MAX_VALUE));
	ConstantStructImpl<IntegerStruct> MOST_NEGATIVE_FIXNUM = ConstantStructImpl.valueOf("MOST-NEGATIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Integer.MIN_VALUE));

	Object EPSILON_PLACEHOLDER = Precision.EPSILON;

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = ConstantStructImpl.valueOf("MOST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = ConstantStructImpl.valueOf("MOST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> SHORT_FLOAT_EPSILON = ConstantStructImpl.valueOf("SHORT-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(5.960465E-8F));
	ConstantStructImpl<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = ConstantStructImpl.valueOf("SHORT-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(2.9802326e-8F));

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = ConstantStructImpl.valueOf("MOST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(Float.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = ConstantStructImpl.valueOf("MOST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(-Float.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> SINGLE_FLOAT_EPSILON = ConstantStructImpl.valueOf("SINGLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(5.960465E-8F));
	ConstantStructImpl<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = ConstantStructImpl.valueOf("SINGLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, SingleFloatStruct.toLispFloat(2.9802326e-8F));

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = ConstantStructImpl.valueOf("MOST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = ConstantStructImpl.valueOf("MOST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> DOUBLE_FLOAT_EPSILON = ConstantStructImpl.valueOf("DOUBLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(1.1102230246251568E-16D));
	ConstantStructImpl<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = ConstantStructImpl.valueOf("DOUBLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(5.551115123125784E-17D));

	ConstantStructImpl<FloatStruct> MOST_POSITIVE_LONG_FLOAT = ConstantStructImpl.valueOf("MOST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = ConstantStructImpl.valueOf("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Double.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = ConstantStructImpl.valueOf("MOST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MAX_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MIN_VALUE));
	ConstantStructImpl<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = ConstantStructImpl.valueOf("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(-Double.MIN_NORMAL));
	ConstantStructImpl<FloatStruct> LONG_FLOAT_EPSILON = ConstantStructImpl.valueOf("LONG-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(1.1102230246251568E-16D));
	ConstantStructImpl<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = ConstantStructImpl.valueOf("LONG-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(5.551115123125784E-17D));

	ConstantStructImpl<FloatStruct> PI = ConstantStructImpl.valueOf("PI", GlobalPackageStruct.COMMON_LISP, DoubleFloatStruct.toLispFloat(Math.PI));
}
