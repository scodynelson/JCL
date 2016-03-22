/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler;

import java.math.BigInteger;

import jcl.lists.ListStruct;
import jcl.numbers.IntIntegerStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.ConstantStruct;

public interface CompilerConstants {

	ConstantStruct<IntegerStruct> CALL_ARGUMENTS_LIMIT = new ConstantStruct<>("CALL-ARGUMENTS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntIntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	// TODO: what is the value for this symbol??
	ConstantStruct<?> ALLOW_OTHER_KEYS = new ConstantStruct<>("&ALLOW-OTHER-KEYS", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> AUX = new ConstantStruct<>("&AUX", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> BODY = new ConstantStruct<>("&BODY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> ENVIRONMENT = new ConstantStruct<>("&ENVIRONMENT", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> KEY = new ConstantStruct<>("&KEY", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> OPTIONAL = new ConstantStruct<>("&OPTIONAL", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> REST = new ConstantStruct<>("&REST", GlobalPackageStruct.COMMON_LISP, null);
	ConstantStruct<?> WHOLE = new ConstantStruct<>("&WHOLE", GlobalPackageStruct.COMMON_LISP, null);

	ConstantStruct<?> LAMBDA_LIST_KEYWORDS = new ConstantStruct<>("LAMBDA-LIST-KEYWORDS", GlobalPackageStruct.COMMON_LISP,
	                                                              ListStruct.buildProperList(
			                                                              ALLOW_OTHER_KEYS,
			                                                              AUX,
			                                                              BODY,
			                                                              ENVIRONMENT,
			                                                              KEY,
			                                                              OPTIONAL,
			                                                              REST,
			                                                              WHOLE));
	ConstantStruct<IntegerStruct> LAMBDA_PARAMETERS_LIMIT = new ConstantStruct<>("LAMBDA-PARAMETERS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntIntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	ConstantStruct<IntegerStruct> MULTIPLE_VALUES_LIMIT = new ConstantStruct<>("MULTIPLE-VALUES-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntIntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	ConstantStruct<IntegerStruct> INTERNAL_TIME_UNITS_PER_SECOND = new ConstantStruct<>("INTERNAL-TIME-UNITS-PER-SECOND", GlobalPackageStruct.COMMON_LISP, new IntIntegerStruct(BigInteger.valueOf(1000000)));
}
