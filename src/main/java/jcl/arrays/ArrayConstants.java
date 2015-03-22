/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays;

import java.math.BigInteger;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Constant;

public interface ArrayConstants {

	Constant<IntegerStruct> ARRAY_DIMENSION_LIMIT = new Constant<>("ARRAY-DIMENSION-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));
	Constant<IntegerStruct> ARRAY_RANK_LIMIT = new Constant<>("ARRAY-RANK-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));
	Constant<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = new Constant<>("ARRAY-TOTAL-SIZE-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Integer.MAX_VALUE)));
}
