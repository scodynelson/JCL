package jcl.functions.list;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class TailpFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "TAILP";
	private static final String OBJECT_ARGUMENT = "OBJECT";
	private static final String LIST_ARGUMENT = "LIST";

	public TailpFunction() {
		super("If object is the same as some tail of list, returns true; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return BooleanStruct.toLispBoolean(list.tailp(object));
	}
}
