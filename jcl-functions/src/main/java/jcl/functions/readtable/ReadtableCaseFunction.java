/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class ReadtableCaseFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "READTABLE-CASE";
	private static final String READTABLE_ARGUMENT = "READTABLE";

	public ReadtableCaseFunction() {
		super("Gets the readtable case of the provided readtable.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(READTABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ReadtableStruct readtable = arguments.getRequiredArgument(READTABLE_ARGUMENT, ReadtableStruct.class);

		final ReadtableCase readtableCase = readtable.getReadtableCase();
		switch (readtableCase) {
			case UPCASE:
				return CommonLispSymbols.UPCASE_KEYWORD;
			case DOWNCASE:
				return CommonLispSymbols.DOWNCASE_KEYWORD;
			case PRESERVE:
				return CommonLispSymbols.PRESERVE_KEYWORD;
			case INVERT:
				return CommonLispSymbols.INVERT_KEYWORD;
		}
		return NILStruct.INSTANCE;
	}
}
