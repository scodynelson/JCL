package jcl.functions.list;

import jcl.functions.SystemBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SetNthFunction extends SystemBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SET-NTH";
	private static final String INDEX_ARGUMENT = "INDEX";
	private static final String LIST_ARGUMENT = "LIST";
	private static final String VALUE_ARGUMENT = "VALUE";

	public SetNthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element, and sets its value to the new-value provided.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INDEX_ARGUMENT)
		                .requiredParameter(LIST_ARGUMENT)
		                .requiredParameter(VALUE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct index = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final LispStruct value = arguments.getRequiredArgument(VALUE_ARGUMENT);

		final long indexValue = index.longValue();
		list.setNth(indexValue, value);
		return value;
	}
}
