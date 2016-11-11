package jcl.functions.sequence;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.functions.SystemBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.SequenceStruct;
import org.springframework.stereotype.Component;

@Component
public final class SetEltFunction extends SystemBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SET-ELT";
	private static final String SEQUENCE_ARGUMENT = "SEQUENCE";
	private static final String INDEX_ARGUMENT = "INDEX";
	private static final String VALUE_ARGUMENT = "VALUE";

	public SetEltFunction() {
		super("Sets the element of sequence specified by index to the new-value provided.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SEQUENCE_ARGUMENT)
		                .requiredParameter(INDEX_ARGUMENT)
		                .requiredParameter(VALUE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SequenceStruct sequence = arguments.getRequiredArgument(SEQUENCE_ARGUMENT, SequenceStruct.class);
		final IntegerStruct index = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);
		final LispStruct value = arguments.getRequiredArgument(VALUE_ARGUMENT);

		final long indexValue = index.longValue();
		sequence.setElt(indexValue, value);
		return value;
	}
}
