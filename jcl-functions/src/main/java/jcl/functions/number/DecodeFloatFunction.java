package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.DecodeFloatResult;
import jcl.lang.number.FloatStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class DecodeFloatFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "DECODE-FLOAT";
	private static final String FLOAT_ARGUMENT = "FLOAT";

	public DecodeFloatFunction() {
		super("Computes three values that characterize float.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FloatStructImpl floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStructImpl.class);
		final DecodeFloatResult decodeFloatResult = floatVal.decodeFloat();
		return ValuesStruct.valueOf(
				decodeFloatResult.getSignificand(),
				decodeFloatResult.getExponent(),
				decodeFloatResult.getSign()
		);
	}
}
