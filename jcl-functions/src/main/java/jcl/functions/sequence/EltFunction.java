package jcl.functions.sequence;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class EltFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "ELT";
	private static final String SEQUENCE_ARGUMENT = "SEQUENCE";
	private static final String INDEX_ARGUMENT = "INDEX";

	public EltFunction() {
		super("Accesses the element of sequence specified by index.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SEQUENCE_ARGUMENT)
		                .requiredParameter(INDEX_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SequenceStruct sequence = arguments.getRequiredArgument(SEQUENCE_ARGUMENT, SequenceStruct.class);
		final IntegerStruct index = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);

		final long indexValue = index.longValue();
		return sequence.elt(indexValue);
	}
}
