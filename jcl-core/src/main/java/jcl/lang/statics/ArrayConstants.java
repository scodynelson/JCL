/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.internal.ConstantStructImpl;

public interface ArrayConstants {

	ConstantStructImpl<IntegerStruct> ARRAY_DIMENSION_LIMIT = ConstantStructImpl.valueOf("ARRAY-DIMENSION-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Integer.MAX_VALUE));
	ConstantStructImpl<IntegerStruct> ARRAY_RANK_LIMIT = ConstantStructImpl.valueOf("ARRAY-RANK-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(8));
	ConstantStructImpl<IntegerStruct> ARRAY_TOTAL_SIZE_LIMIT = ConstantStructImpl.valueOf("ARRAY-TOTAL-SIZE-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Integer.MAX_VALUE));
}
