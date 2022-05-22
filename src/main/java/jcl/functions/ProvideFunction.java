package jcl.functions;

import jcl.compiler.function.InternalModuleHandler;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ProvideFunction extends BuiltInFunctionStructImpl {

	private static final String MODULE_NAME_ARGUMENT = "MODULE-NAME";

	public ProvideFunction() {
		super("Adds the module-name to the list held by *modules*, if such a name is not already present.",
		      CommonLispSymbols.PROVIDE.getName(),
		      Parameters.forFunction(CommonLispSymbols.PROVIDE.getName())
		                .requiredParameter(MODULE_NAME_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.PROVIDE;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct moduleName = arguments.getRequiredArgument(MODULE_NAME_ARGUMENT, LispStruct.class);
		final StringStruct moduleNameString = StringStruct.fromDesignator(moduleName);

		return InternalModuleHandler.provide(moduleNameString);
	}
}
