package jcl.functions.list;

import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code consp}.
 */
@Component
public final class ConsPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CONSP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public ConsPFunction() {
		super("Returns true if object is of type cons; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return LispStructFactory.toBoolean(object instanceof ConsStruct);
	}
}
