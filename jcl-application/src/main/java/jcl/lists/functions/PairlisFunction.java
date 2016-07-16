package jcl.lists.functions;

import jcl.LispStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.NILStruct;
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
