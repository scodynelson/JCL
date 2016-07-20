/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.array;

import java.math.BigInteger;

import jcl.lang.ConstantStruct;
import jcl.lang.GlobalPackageStruct;
import jcl.lang.number.IntegerStruct;

public interface ArrayConstants {

	ConstantStruct<IntegerStruct> ARRAY_DIMENSION_LIMIT = new ConstantStruct<>("ARRAY-DIMENSION-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
	ConstantStruct<IntegerStruct> ARRAY_RANK_LIMIT = new ConstantStruct<>("ARRAY-RANK-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Short.MAX_VALUE)));
	ConstantStruct<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = new ConstantStruct<>("ARRAY-TOTAL-SIZE-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Integer.MAX_VALUE)));
}
