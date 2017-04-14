package jcl.functions.condition;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public class ErrorFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "ERROR";
	private static final String DATUM_ARGUMENT = "DATUM";

	public ErrorFunction() {
		super("Error effectively invokes signal on the denoted condition.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(DATUM_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct datum = arguments.getRequiredArgument(DATUM_ARGUMENT, StringStruct.class);
		// TODO: Fix this when we actually create the condition system.
		throw new ErrorException(datum.toJavaString());
	}
}
