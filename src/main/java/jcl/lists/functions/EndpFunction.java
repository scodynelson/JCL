package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
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
		return BooleanStructs.toLispBoolean(NILStruct.INSTANCE.equals(list));
	}
}
