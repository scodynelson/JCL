package jcl.functions.list;

import jcl.lang.list.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class PairlisFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PAIRLIS";
	private static final String KEYS_ARGUMENT = "KEYS";
	private static final String DATUMS_ARGUMENT = "DATUMS";
	private static final String ALIST_ARGUMENT = "ALIST";

	public PairlisFunction() {
		super("Returns an association list that associates elements of keys to corresponding elements of data.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(KEYS_ARGUMENT)
		                .requiredParameter(DATUMS_ARGUMENT)
		                .optionalParameter(ALIST_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct keys = arguments.getRequiredArgument(KEYS_ARGUMENT, ListStruct.class);
		final ListStruct datums = arguments.getRequiredArgument(DATUMS_ARGUMENT, ListStruct.class);

		final long keysLength = keys.length();
		final long datumsLength = datums.length();
		if (keysLength != datumsLength) {
			throw new SimpleErrorException("The lists of keys and datums are not the same length.");
		}

		final LispStruct[] keysArray = keys.toArray();
		final LispStruct[] datumsArray = datums.toArray();

		ListStruct alist = arguments.getOptionalArgument(ALIST_ARGUMENT, ListStruct.class);

		for (int i = 0; i < keysLength; i++) {
			final LispStruct key = keysArray[i];
			final LispStruct datum = datumsArray[i];
			final ConsStruct pair = new ConsStruct(key, datum);
			alist = new ConsStruct(pair, alist);
		}

		return alist;
	}
}
