package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.FloatStruct;
import org.springframework.stereotype.Component;

@Component
public final class FloatDigitsFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FLOAT-DIGITS";
	private static final String FLOAT_ARGUMENT = "FLOAT";

	public FloatDigitsFunction() {
		super("Returns the number of radix b digits used in the representation of float.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FloatStruct floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStruct.class);
		return floatVal.floatDigits();
	}
}
