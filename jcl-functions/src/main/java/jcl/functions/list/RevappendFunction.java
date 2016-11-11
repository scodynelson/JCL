package jcl.functions.list;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class RevappendFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "REVAPPEND";
	private static final String LIST_ARGUMENT = "LIST";
	private static final String TAIL_ARGUMENT = "TAIL";

	public RevappendFunction() {
		super("Constructs a copy of list, but with the elements in reverse order. It then appends (as if by nconc) the tail to that reversed list and returns the result.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		                .requiredParameter(TAIL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final LispStruct tail = arguments.getRequiredArgument(TAIL_ARGUMENT);
		return list.revAppend(tail);
	}
}
