package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public final class CopyAlistFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COPY-ALIST";
	private static final String ALIST_ARGUMENT = "ALIST";

	public CopyAlistFunction() {
		super("Returns a copy of alist.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(ALIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(ALIST_ARGUMENT, ListStruct.class);
		return list.copyAlist();
	}
}
