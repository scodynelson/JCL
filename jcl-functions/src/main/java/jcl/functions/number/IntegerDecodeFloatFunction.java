package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.DecodeFloatResult;
import jcl.lang.number.FloatStruct;
import org.springframework.stereotype.Component;

@Component
public final class IntegerDecodeFloatFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "INTEGER-DECODE-FLOAT";
	private static final String FLOAT_ARGUMENT = "FLOAT";

	public IntegerDecodeFloatFunction() {
		super("Computes three values that characterize float.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FloatStruct floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStruct.class);
		final DecodeFloatResult decodeFloatResult = floatVal.integerDecodeFloat();
		return ValuesStruct.valueOf(
				decodeFloatResult.getSignificand(),
				decodeFloatResult.getExponent(),
				decodeFloatResult.getSign()
		);
	}
}
