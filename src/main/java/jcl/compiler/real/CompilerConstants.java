/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real;

import java.math.BigInteger;

import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Constant;

public interface CompilerConstants {

	Constant<IntegerStruct> CALL_ARGUMENTS_LIMIT = new Constant<>("CALL-ARGUMENTS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	// TODO: what is the value for this symbol??
	Constant<?> ALLOW_OTHER_KEYS = new Constant<>("&ALLOW-OTHER-KEYS", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> AUX = new Constant<>("&AUX", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> BODY = new Constant<>("&BODY", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> ENVIRONMENT = new Constant<>("&ENVIRONMENT", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> KEY = new Constant<>("&KEY", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> OPTIONAL = new Constant<>("&OPTIONAL", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> REST = new Constant<>("&REST", GlobalPackageStruct.COMMON_LISP, null);
	Constant<?> WHOLE = new Constant<>("&WHOLE", GlobalPackageStruct.COMMON_LISP, null);

	Constant<?> LAMBDA_LIST_KEYWORDS = new Constant<>("LAMBDA-LIST-KEYWORDS", GlobalPackageStruct.COMMON_LISP,
			ListStruct.buildProperList(
					ALLOW_OTHER_KEYS,
					AUX,
					BODY,
					ENVIRONMENT,
					KEY,
					OPTIONAL,
					REST,
					WHOLE));
	Constant<IntegerStruct> LAMBDA_PARAMETERS_LIMIT = new Constant<>("LAMBDA-PARAMETERS-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	Constant<IntegerStruct> MULTIPLE_VALUES_LIMIT = new Constant<>("MULTIPLE-VALUES-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Short.MAX_VALUE)));

	Constant<IntegerStruct> INTERNAL_TIME_UNITS_PER_SECOND = new Constant<>("INTERNAL-TIME-UNITS-PER-SECOND", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(1000000)));
}
