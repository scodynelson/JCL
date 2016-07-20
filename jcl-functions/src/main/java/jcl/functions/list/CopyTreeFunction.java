package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import org.springframework.stereotype.Component;

@Component
public final class CopyTreeFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COPY-TREE";
	private static final String LIST_ARGUMENT = "LIST";

	public CopyTreeFunction() {
		super("Creates a copy of a tree of conses.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return list.copyTree();
	}
}
