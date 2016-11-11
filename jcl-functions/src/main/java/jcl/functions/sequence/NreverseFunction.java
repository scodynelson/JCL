package jcl.functions.sequence;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.SequenceStruct;
import org.springframework.stereotype.Component;

@Component
public final class NreverseFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "NREVERSE";
	private static final String SEQUENCE_ARGUMENT = "SEQUENCE";

	public NreverseFunction() {
		super("Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order; the original sequence may be modified.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SEQUENCE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SequenceStruct sequence = arguments.getRequiredArgument(SEQUENCE_ARGUMENT, SequenceStruct.class);
		return sequence.nReverse();
	}
}
