/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.ComplexStructImpl;
import jcl.lang.number.FloatStructImpl;
import jcl.lang.number.IntegerStructImpl;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
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
		                .optionalParameter(IMAGPART_ARGUMENT).withInitialValue(IntegerStructImpl.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(IMAGPART_ARGUMENT)) {
			final RealStruct real = arguments.getRequiredArgument(REALPART_ARGUMENT, RealStruct.class);
			final RealStruct imaginary = arguments.getOptionalArgument(IMAGPART_ARGUMENT, RealStruct.class);

			return ComplexStructImpl.valueOf(real, imaginary);
		} else {
			final RealStruct real = arguments.getRequiredArgument(REALPART_ARGUMENT, RealStruct.class);
			if (real instanceof RationalStruct) {
				return real;
			} else {
				return ComplexStructImpl.valueOf(real, FloatStructImpl.ZERO);
			}
		}
	}
}
