package jcl.functions.list;

import jcl.lang.list.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class RplacdFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RPLACD";
	private static final String CONS_ARGUMENT = "CONS";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public RplacdFunction() {
		super("Replaces the cdr of the cons with object.",
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
		cons.setCdr(object);
		return cons;
	}
}
