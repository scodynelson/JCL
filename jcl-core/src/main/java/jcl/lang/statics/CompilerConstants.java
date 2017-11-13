/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.ListStruct;
import jcl.lang.internal.ConstantStructImpl;

public interface CompilerConstants {

	ConstantStructImpl<IntegerStruct> CALL_ARGUMENTS_LIMIT = ConstantStructImpl.valueOf("CALL-ARGUMENTS-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Short.MAX_VALUE));

	// TODO: what is the value for this symbol??
	ConstantStructImpl<?> ALLOW_OTHER_KEYS = ConstantStructImpl.valueOf("&ALLOW-OTHER-KEYS", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> AUX = ConstantStructImpl.valueOf("&AUX", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> BODY = ConstantStructImpl.valueOf("&BODY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> ENVIRONMENT = ConstantStructImpl.valueOf("&ENVIRONMENT", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> KEY = ConstantStructImpl.valueOf("&KEY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> OPTIONAL = ConstantStructImpl.valueOf("&OPTIONAL", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> REST = ConstantStructImpl.valueOf("&REST", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStructImpl<?> WHOLE = ConstantStructImpl.valueOf("&WHOLE", GlobalPackageStruct.COMMON_LISP, null);

	ConstantStructImpl<?> LAMBDA_LIST_KEYWORDS = ConstantStructImpl.valueOf("LAMBDA-LIST-KEYWORDS", GlobalPackageStruct.COMMON_LISP,
	                                                                        ListStruct.toLispList(
			                                                              ALLOW_OTHER_KEYS,
			                                                              AUX,
			                                                              BODY,
			                                                              ENVIRONMENT,
			                                                              KEY,
			                                                              OPTIONAL,
			                                                              REST,
			                                                              WHOLE));
	ConstantStructImpl<IntegerStruct> LAMBDA_PARAMETERS_LIMIT = ConstantStructImpl.valueOf("LAMBDA-PARAMETERS-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Short.MAX_VALUE));

	ConstantStructImpl<IntegerStruct> MULTIPLE_VALUES_LIMIT = ConstantStructImpl.valueOf("MULTIPLE-VALUES-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(Short.MAX_VALUE));

	ConstantStructImpl<IntegerStruct> INTERNAL_TIME_UNITS_PER_SECOND = ConstantStructImpl.valueOf("INTERNAL-TIME-UNITS-PER-SECOND", GlobalPackageStruct.COMMON_LISP, IntegerStruct.toLispInteger(1000000));
}
