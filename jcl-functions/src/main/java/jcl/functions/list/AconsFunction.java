package jcl.functions.list;

import jcl.lang.list.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class AconsFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ACONS";
	private static final String KEY_ARGUMENT = "KEY";
	private static final String DATUM_ARGUMENT = "DATUM";
	private static final String ALIST_ARGUMENT = "ALIST";

	public AconsFunction() {
		super("Creates a fresh cons, the cdr of which is alist and the car of which is another fresh cons, the car of which is key and the cdr of which is datum.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(KEY_ARGUMENT)
		                .requiredParameter(DATUM_ARGUMENT)
		                .requiredParameter(ALIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct key = arguments.getRequiredArgument(KEY_ARGUMENT);
		final LispStruct datum = arguments.getRequiredArgument(DATUM_ARGUMENT);
		final ListStruct alist = arguments.getRequiredArgument(ALIST_ARGUMENT, ListStruct.class);

		final ConsStruct pair = new ConsStruct(key, datum);
		return new ConsStruct(pair, alist);
	}
}
