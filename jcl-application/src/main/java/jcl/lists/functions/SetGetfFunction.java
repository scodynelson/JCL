package jcl.lists.functions;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.SystemBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SetGetfFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SET-GETF";
	private static final String PLIST_ARGUMENT = "PLIST";
	private static final String INDICATOR_ARGUMENT = "INDICATOR";
	private static final String VALUE_ARGUMENT = "VALUE";
	private static final String DEFAULT_ARGUMENT = "DEFAULT";

	public SetGetfFunction() {
		super("Finds a property on the property list whose property indicator is identical to indicator, and sets its corresponding property value with the new-value provided.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PLIST_ARGUMENT)
		                .requiredParameter(INDICATOR_ARGUMENT)
		                .requiredParameter(VALUE_ARGUMENT)
		                .optionalParameter(DEFAULT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct plist = arguments.getRequiredArgument(PLIST_ARGUMENT, ListStruct.class);
		final LispStruct indicator = arguments.getRequiredArgument(INDICATOR_ARGUMENT);
		final LispStruct value = arguments.getRequiredArgument(VALUE_ARGUMENT);

		plist.setProperty(indicator, value);
		return value;
	}
}
