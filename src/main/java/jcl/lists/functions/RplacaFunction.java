package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ConsStruct;
import org.springframework.stereotype.Component;

@Component
public final class RplacaFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RPLACA";
	private static final String CONS_ARGUMENT = "CONS";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public RplacaFunction() {
		super("Replaces the car of the cons with object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CONS_ARGUMENT)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ConsStruct cons = arguments.getRequiredArgument(CONS_ARGUMENT, ConsStruct.class);
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		cons.setCar(object);
		return cons;
	}
}
