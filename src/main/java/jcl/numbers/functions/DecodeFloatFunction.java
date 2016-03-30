package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.DecodeFloatResult;
import jcl.numbers.FloatStruct;
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
		final FloatStruct floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStruct.class);
		final DecodeFloatResult decodeFloatResult = floatVal.decodeFloat();
		return new ValuesStruct(
				decodeFloatResult.getSignificand(),
				decodeFloatResult.getExponent(),
				decodeFloatResult.getSign()
		);
	}
}
