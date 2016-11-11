package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class EndpFunction extends CommonLispBuiltInFunctionStructBase {

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
		return LispStructFactory.toBoolean(NILStruct.INSTANCE.equals(list));
	}
}
