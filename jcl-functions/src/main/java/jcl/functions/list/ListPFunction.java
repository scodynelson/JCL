package jcl.functions.list;

import jcl.lang.BooleanStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code listp}.
 */
@Component
public final class ListPFunction extends CommonLispBuiltInFunctionStruct {

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
		return BooleanStructImpl.toLispBoolean(object instanceof ListStruct);
	}
}
