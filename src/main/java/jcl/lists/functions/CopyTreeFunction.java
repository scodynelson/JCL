package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
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
