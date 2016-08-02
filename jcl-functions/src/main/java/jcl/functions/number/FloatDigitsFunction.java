package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.FloatStructImpl;
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
		final FloatStructImpl floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStructImpl.class);
		return floatVal.floatDigits();
	}
}
