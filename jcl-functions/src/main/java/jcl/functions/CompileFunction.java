package jcl.functions;

import jcl.compiler.function.InternalCompile;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class CompileFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COMPILE";
	private static final String NAME_ARGUMENT = "NAME";
	private static final String DEFINITION_ARGUMENT = "DEFINITION";

	public CompileFunction() {
		super("Produces a compiled function from definition.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NAME_ARGUMENT)
		                .optionalParameter(DEFINITION_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct name = arguments.getRequiredArgument(NAME_ARGUMENT);
		LispStruct uncompiledDefinition = null;
		if (arguments.hasOptionalArgument(DEFINITION_ARGUMENT)) {
			uncompiledDefinition = arguments.getOptionalArgument(DEFINITION_ARGUMENT);
		}

		return InternalCompile.compile(name, uncompiledDefinition);
	}
}
