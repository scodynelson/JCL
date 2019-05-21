package jcl.functions;

import jcl.compiler.function.InternalModuleHandler;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class RequireFunction extends BuiltInFunctionStructImpl {

	private static final String MODULE_NAME_ARGUMENT = "MODULE-NAME";
	private static final String PATHNAME_LIST_ARGUMENT = "PATHNAME-LIST";

	public RequireFunction() {
		super("Tests for the presence of the module-name in the list held by *modules*. If it is present, require immediately returns. Otherwise, an attempt is made to load an appropriate set of files.",
		      CommonLispSymbols.REQUIRE.getName(),
		      Parameters.forFunction(CommonLispSymbols.REQUIRE.getName())
		                .requiredParameter(MODULE_NAME_ARGUMENT)
		                .optionalParameter(PATHNAME_LIST_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.REQUIRE;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct moduleName = arguments.getRequiredArgument(MODULE_NAME_ARGUMENT, LispStruct.class);
		final StringStruct moduleNameString = StringStruct.toLispString(moduleName);

		final ListStruct pathnameList = arguments.getOptionalArgument(PATHNAME_LIST_ARGUMENT, ListStruct.class);

		return InternalModuleHandler.require(moduleNameString, pathnameList);
	}
}
