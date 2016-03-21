package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class NthcdrFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "NTHCDR";
	private static final String N_ARGUMENT = "N";
	private static final String LIST_ARGUMENT = "LIST";

	public NthcdrFunction() {
		super("Returns the tail of list that would be obtained by calling cdr n times in succession.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(N_ARGUMENT)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct nVal = arguments.getRequiredArgument(N_ARGUMENT, IntegerStruct.class);
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);

		final long nLong = nVal.getBigInteger().longValue();
		return list.nthCdr(nLong);
	}
}
