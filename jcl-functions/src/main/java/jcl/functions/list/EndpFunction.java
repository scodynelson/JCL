package jcl.functions.list;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class EndpFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ENDP";
	private static final String LIST_ARGUMENT = "LIST";

	public EndpFunction() {
		super("Returns true if list is the empty list. Returns false if list is a cons.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return BooleanStruct.toLispBoolean(NILStruct.INSTANCE.equals(list));
	}
}
