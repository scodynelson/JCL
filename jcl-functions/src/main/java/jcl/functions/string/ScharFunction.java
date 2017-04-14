package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ScharFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SCHAR";
	private static final String STRING_ARGUMENT = "STRING";
	private static final String INDEX_ARGUMENT = "INDEX";

	public ScharFunction() {
		super("Schar accesses the element of string specified by index.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_ARGUMENT)
		                .requiredParameter(INDEX_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct struct = arguments.getRequiredArgument(STRING_ARGUMENT, StringStruct.class);
		final IntegerStruct index = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);
		return struct.schar(index);
	}
}
