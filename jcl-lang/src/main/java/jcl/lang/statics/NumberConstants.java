/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.internal.ConstantStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.internal.number.FloatStructImpl;
import jcl.lang.internal.number.IntegerStructImpl;
import org.apache.commons.math3.util.Precision;

public interface NumberConstants {

	ConstantStruct<IntegerStruct> BOOLE_1 = ConstantStruct.valueOf("BOOLE-1", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
	ConstantStruct<IntegerStruct> BOOLE_2 = ConstantStruct.valueOf("BOOLE-2", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ONE);
	ConstantStruct<IntegerStruct> BOOLE_AND = ConstantStruct.valueOf("BOOLE-AND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.TWO);
	ConstantStruct<IntegerStruct> BOOLE_ANDC1 = ConstantStruct.valueOf("BOOLE-ANDC1", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(3)));
	ConstantStruct<IntegerStruct> BOOLE_ANDC2 = ConstantStruct.valueOf("BOOLE-ANDC2", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(4)));
	ConstantStruct<IntegerStruct> BOOLE_C1 = ConstantStruct.valueOf("BOOLE-C1", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(5)));
	ConstantStruct<IntegerStruct> BOOLE_C2 = ConstantStruct.valueOf("BOOLE-C2", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(6)));
	ConstantStruct<IntegerStruct> BOOLE_CLR = ConstantStruct.valueOf("BOOLE-CLR", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(7)));
	ConstantStruct<IntegerStruct> BOOLE_EQV = ConstantStruct.valueOf("BOOLE-EQV", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(8)));
	ConstantStruct<IntegerStruct> BOOLE_IOR = ConstantStruct.valueOf("BOOLE-IOR", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(9)));
	ConstantStruct<IntegerStruct> BOOLE_NAND = ConstantStruct.valueOf("BOOLE-NAND", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.TEN));
	ConstantStruct<IntegerStruct> BOOLE_NOR = ConstantStruct.valueOf("BOOLE-NOR", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(11)));
	ConstantStruct<IntegerStruct> BOOLE_ORC1 = ConstantStruct.valueOf("BOOLE-ORC1", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(12)));
	ConstantStruct<IntegerStruct> BOOLE_ORC2 = ConstantStruct.valueOf("BOOLE-ORC2", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(13)));
	ConstantStruct<IntegerStruct> BOOLE_SET = ConstantStruct.valueOf("BOOLE-SET", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(14)));
	ConstantStruct<IntegerStruct> BOOLE_XOR = ConstantStruct.valueOf("BOOLE-XOR", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(15)));

	ConstantStruct<IntegerStruct> MOST_POSITIVE_FIXNUM = ConstantStruct.valueOf("MOST-POSITIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
	ConstantStruct<IntegerStruct> MOST_NEGATIVE_FIXNUM = ConstantStruct.valueOf("MOST-NEGATIVE-FIXNUM", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(Integer.MIN_VALUE)));

	Object EPSILON_PLACEHOLDER = Precision.EPSILON;

	ConstantStruct<FloatStruct> MOST_POSITIVE_SHORT_FLOAT = ConstantStruct.valueOf("MOST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_SHORT_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MIN_NORMAL));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_SHORT_FLOAT = ConstantStruct.valueOf("MOST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_SHORT_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MIN_NORMAL));
	ConstantStruct<FloatStruct> SHORT_FLOAT_EPSILON = ConstantStruct.valueOf("SHORT-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(5.960465E-8F));
	ConstantStruct<FloatStruct> SHORT_FLOAT_NEGATIVE_EPSILON = ConstantStruct.valueOf("SHORT-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(2.9802326e-8F));

	ConstantStruct<FloatStruct> MOST_POSITIVE_SINGLE_FLOAT = ConstantStruct.valueOf("MOST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_SINGLE_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Float.MIN_NORMAL));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_SINGLE_FLOAT = ConstantStruct.valueOf("MOST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_SINGLE_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Float.MIN_NORMAL));
	ConstantStruct<FloatStruct> SINGLE_FLOAT_EPSILON = ConstantStruct.valueOf("SINGLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(5.960465E-8F));
	ConstantStruct<FloatStruct> SINGLE_FLOAT_NEGATIVE_EPSILON = ConstantStruct.valueOf("SINGLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(2.9802326e-8F));

	ConstantStruct<FloatStruct> MOST_POSITIVE_DOUBLE_FLOAT = ConstantStruct.valueOf("MOST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_DOUBLE_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MIN_NORMAL));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_DOUBLE_FLOAT = ConstantStruct.valueOf("MOST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_DOUBLE_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MIN_NORMAL));
	ConstantStruct<FloatStruct> DOUBLE_FLOAT_EPSILON = ConstantStruct.valueOf("DOUBLE-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(1.1102230246251568E-16));
	ConstantStruct<FloatStruct> DOUBLE_FLOAT_NEGATIVE_EPSILON = ConstantStruct.valueOf("DOUBLE-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(5.551115123125784E-17));

	ConstantStruct<FloatStruct> MOST_POSITIVE_LONG_FLOAT = ConstantStruct.valueOf("MOST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_LONG_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_POSITIVE_NORMALIZED_LONG_FLOAT = ConstantStruct.valueOf("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Double.MIN_NORMAL));
	ConstantStruct<FloatStruct> MOST_NEGATIVE_LONG_FLOAT = ConstantStruct.valueOf("MOST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MAX_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_LONG_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MIN_VALUE));
	ConstantStruct<FloatStruct> LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT = ConstantStruct.valueOf("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(-Double.MIN_NORMAL));
	ConstantStruct<FloatStruct> LONG_FLOAT_EPSILON = ConstantStruct.valueOf("LONG-FLOAT-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(1.1102230246251568E-16));
	ConstantStruct<FloatStruct> LONG_FLOAT_NEGATIVE_EPSILON = ConstantStruct.valueOf("LONG-FLOAT-NEGATIVE-EPSILON", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(5.551115123125784E-17));

	ConstantStruct<FloatStruct> PI = ConstantStruct.valueOf("PI", GlobalPackageStruct.COMMON_LISP, FloatStructImpl.valueOf(Math.PI));
}
