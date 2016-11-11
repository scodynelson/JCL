package jcl.functions.list;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code listp}.
 */
@Component
public final class ListPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LISTP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public ListPFunction() {
		super("Returns true if object is of type list; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return LispStructFactory.toBoolean(object instanceof ListStruct);
	}
}
