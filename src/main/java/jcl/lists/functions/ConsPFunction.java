package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ConsStruct;
import jcl.symbols.BooleanStructs;
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
		return BooleanStructs.toLispBoolean(object instanceof ConsStruct);
	}
}
