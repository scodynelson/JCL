package jcl.functions;

import jcl.compiler.function.InternalEval;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class EvalFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "EVAL";
	private static final String FORM_ARGUMENT = "FORM";

	public EvalFunction() {
		super("Evaluates form in the current dynamic environment and the null lexical environment.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FORM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		return InternalEval.eval(form);
	}
}
