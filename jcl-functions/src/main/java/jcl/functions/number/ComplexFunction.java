/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ComplexFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COMPLEX";
	private static final String REALPART_ARGUMENT = "REALPART";
	private static final String IMAGPART_ARGUMENT = "IMAGPART";

	public ComplexFunction() {
		super("Returns a number whose real part is realpart and whose imaginary part is imagpart.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REALPART_ARGUMENT)
		                .optionalParameter(IMAGPART_ARGUMENT).withInitialValue(IntegerStruct.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(IMAGPART_ARGUMENT)) {
			final RealStruct real = arguments.getRequiredArgument(REALPART_ARGUMENT, RealStruct.class);
			final RealStruct imaginary = arguments.getOptionalArgument(IMAGPART_ARGUMENT, RealStruct.class);

			return LispStructFactory.toComplex(real, imaginary);
		} else {
			final RealStruct real = arguments.getRequiredArgument(REALPART_ARGUMENT, RealStruct.class);
			if (real instanceof RationalStruct) {
				return real;
			} else {
				return LispStructFactory.toComplex(real, FloatStruct.ZERO);
			}
		}
	}
}
