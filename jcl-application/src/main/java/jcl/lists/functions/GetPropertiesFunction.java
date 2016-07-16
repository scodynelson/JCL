package jcl.lists.functions;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class GetPropertiesFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GET-PROPERTIES";
	private static final String PLIST_ARGUMENT = "PLIST";
	private static final String INDICATOR_LIST_ARGUMENT = "INDICATOR-LIST";

	public GetPropertiesFunction() {
		super("Used to look up any of several property list entries all at once.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PLIST_ARGUMENT)
		                .requiredParameter(INDICATOR_LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct plist = arguments.getRequiredArgument(PLIST_ARGUMENT, ListStruct.class);
		final ListStruct indicatorList = arguments.getRequiredArgument(INDICATOR_LIST_ARGUMENT, ListStruct.class);
		return plist.getProperties(indicatorList);
	}
}
