package jcl.functions.list;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class CopyAlistFunction extends CommonLispBuiltInFunctionStructBase {

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
