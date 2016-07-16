package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.SystemBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.symbols.NILStruct;
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
