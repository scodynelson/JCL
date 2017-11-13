package jcl.functions.list;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.FixnumStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class NthcdrFunction extends CommonLispBuiltInFunctionStructBase {

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
		final FixnumStruct nVal = arguments.getRequiredArgument(N_ARGUMENT, FixnumStruct.class);
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return list.nthCdr(nVal);
	}
}
