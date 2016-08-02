package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class LastFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LAST";
	private static final String LIST_ARGUMENT = "LIST";
	private static final String N_ARGUMENT = "N";

	public LastFunction() {
		super("Returns the last n conses (not the last n elements) of list).",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		                .optionalParameter(N_ARGUMENT).withInitialValue(IntegerStructImpl.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final long nVal = arguments.getOptionalArgument(N_ARGUMENT, IntegerStructImpl.class).longValue();
		return list.last(nVal);
	}
}
