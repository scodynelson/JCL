package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class NbutlastFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "NBUTLAST";
	private static final String LIST_ARGUMENT = "LIST";
	private static final String N_ARGUMENT = "N";

	public NbutlastFunction() {
		super("Returns a list from which the last n conses have been omitted. The provided list may be modified.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		                .optionalParameter(N_ARGUMENT).withInitialValue(IntegerStruct.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final IntegerStruct n = arguments.getOptionalArgument(N_ARGUMENT, IntegerStruct.class);
		return list.nButLast(n.longValue());
	}
}
