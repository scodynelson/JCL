/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.ConstantStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.number.IntegerStructImpl;

public interface CompilerConstants {

	ConstantStruct<IntegerStructImpl> CALL_ARGUMENTS_LIMIT = ConstantStruct.valueOf("CALL-ARGUMENTS-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(Short.MAX_VALUE)));

	// TODO: what is the value for this symbol??
	ConstantStruct<?> ALLOW_OTHER_KEYS = ConstantStruct.valueOf("&ALLOW-OTHER-KEYS", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> AUX = ConstantStruct.valueOf("&AUX", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> BODY = ConstantStruct.valueOf("&BODY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> ENVIRONMENT = ConstantStruct.valueOf("&ENVIRONMENT", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> KEY = ConstantStruct.valueOf("&KEY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> OPTIONAL = ConstantStruct.valueOf("&OPTIONAL", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> REST = ConstantStruct.valueOf("&REST", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> WHOLE = ConstantStruct.valueOf("&WHOLE", GlobalPackageStruct.COMMON_LISP, null);

	ConstantStruct<?> LAMBDA_LIST_KEYWORDS = ConstantStruct.valueOf("LAMBDA-LIST-KEYWORDS", GlobalPackageStruct.COMMON_LISP,
	                                                              LispStructFactory.toProperList(
			                                                              ALLOW_OTHER_KEYS,
			                                                              AUX,
			                                                              BODY,
			                                                              ENVIRONMENT,
			                                                              KEY,
			                                                              OPTIONAL,
			                                                              REST,
			                                                              WHOLE));
	ConstantStruct<IntegerStructImpl> LAMBDA_PARAMETERS_LIMIT = ConstantStruct.valueOf("LAMBDA-PARAMETERS-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(Short.MAX_VALUE)));

	ConstantStruct<IntegerStructImpl> MULTIPLE_VALUES_LIMIT = ConstantStruct.valueOf("MULTIPLE-VALUES-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(Short.MAX_VALUE)));

	ConstantStruct<IntegerStructImpl> INTERNAL_TIME_UNITS_PER_SECOND = ConstantStruct.valueOf("INTERNAL-TIME-UNITS-PER-SECOND", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.valueOf(BigInteger.valueOf(1000000)));
}
