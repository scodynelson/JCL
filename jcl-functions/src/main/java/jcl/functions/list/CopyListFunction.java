package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.stereotype.Component;

@Component
public final class CopyListFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COPY-LIST";
	private static final String LIST_ARGUMENT = "LIST";

	public CopyListFunction() {
		super("Returns a copy of list.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return list.copyList();
	}
}
