package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class GetfFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GETF";
	private static final String PLIST_ARGUMENT = "PLIST";
	private static final String INDICATOR_ARGUMENT = "INDICATOR";
	private static final String DEFAULT_ARGUMENT = "DEFAULT";

	public GetfFunction() {
		super("Finds a property on the property list whose property indicator is identical to indicator, and returns its corresponding property value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PLIST_ARGUMENT)
		                .requiredParameter(INDICATOR_ARGUMENT)
		                .optionalParameter(DEFAULT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct plist = arguments.getRequiredArgument(PLIST_ARGUMENT, ListStruct.class);
		final LispStruct indicator = arguments.getRequiredArgument(INDICATOR_ARGUMENT);
		final LispStruct defaultValue = arguments.getOptionalArgument(DEFAULT_ARGUMENT);
		return plist.getProperty(indicator, defaultValue);
	}
}
