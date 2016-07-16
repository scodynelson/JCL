package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.symbols.BooleanStructs;
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
		return BooleanStructs.toLispBoolean(list.tailp(object));
	}
}
