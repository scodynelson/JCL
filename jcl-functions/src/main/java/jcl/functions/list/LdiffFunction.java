package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.stereotype.Component;

@Component
public final class LdiffFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LDIFF";
	private static final String LIST_ARGUMENT = "LIST";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public LdiffFunction() {
		super("If object is the same as some tail of list, returns a fresh list of the elements of list that precede object in the list structure of list; otherwise, it returns a copy of list.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return list.ldiff(object);
	}
}
