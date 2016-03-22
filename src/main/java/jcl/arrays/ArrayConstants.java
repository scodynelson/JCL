/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays;

import java.math.BigInteger;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.ConstantStruct;

public interface ArrayConstants {

	ConstantStruct<IntegerStruct> ARRAY_DIMENSION_LIMIT = new ConstantStruct<>("ARRAY-DIMENSION-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
	ConstantStruct<IntegerStruct> ARRAY_RANK_LIMIT = new ConstantStruct<>("ARRAY-RANK-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Short.MAX_VALUE)));
	ConstantStruct<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = new ConstantStruct<>("ARRAY-TOTAL-SIZE-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
}
