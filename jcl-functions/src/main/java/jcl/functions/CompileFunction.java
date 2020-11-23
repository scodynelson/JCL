package jcl.functions;

import jcl.compiler.function.CompileResult;
import jcl.compiler.function.InternalCompile;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class CompileFunction extends BuiltInFunctionStructImpl {

	private static final String NAME_ARGUMENT = "NAME";
	private static final String DEFINITION_ARGUMENT = "DEFINITION";

	public CompileFunction() {
		super("Produces a compiled function from definition.",
		      CommonLispSymbols.COMPILE.getName(),
		      Parameters.forFunction(CommonLispSymbols.COMPILE.getName())
		                .requiredParameter(NAME_ARGUMENT)
		                .optionalParameter(DEFINITION_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.COMPILE;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct name = arguments.getRequiredArgument(NAME_ARGUMENT);
		LispStruct uncompiledDefinition = null;
		if (arguments.hasOptionalArgument(DEFINITION_ARGUMENT)) {
			uncompiledDefinition = arguments.getOptionalArgument(DEFINITION_ARGUMENT);
		}

		final CompileResult compileResult = InternalCompile.compile(name, uncompiledDefinition);
		return compileResult.toValues();
	}
}
