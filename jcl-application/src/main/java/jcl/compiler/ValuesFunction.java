/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ValuesFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "VALUES";

	public ValuesFunction() {
		super("Returns the objects as multiple values.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> valuesList = arguments.getRestArgument();
		return new ValuesStruct(valuesList);
	}
}
