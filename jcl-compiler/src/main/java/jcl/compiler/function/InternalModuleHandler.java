package jcl.compiler.function;

import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class InternalModuleHandler {

	public static BooleanStruct provide(final StringStruct moduleName) {
		final ListStruct currentModulesList = CommonLispSymbols.MODULES_VAR.getVariableValue();

		if (currentModulesList == NILStruct.INSTANCE) {
			CommonLispSymbols.MODULES_VAR.setValue(ListStruct.toLispList(moduleName));
		} else {
			final boolean moduleAlreadyExists = moduleAlreadyExists(moduleName);
			if (moduleAlreadyExists) {
				return NILStruct.INSTANCE;
			}

			final ListStruct newModulesList = ConsStruct.toLispCons(moduleName, currentModulesList);
			CommonLispSymbols.MODULES_VAR.setValue(newModulesList);
		}
		return TStruct.INSTANCE;
	}

	public static BooleanStruct require(final StringStruct moduleName, final ListStruct pathnameList) {
		final boolean moduleAlreadyExists = moduleAlreadyExists(moduleName);
		if (moduleAlreadyExists) {
			return NILStruct.INSTANCE;
		}

		if (pathnameList == NILStruct.INSTANCE) {
			InternalLoad.loadJavaModule(moduleName);
		} else {
			for (final LispStruct pathname : pathnameList) {
				final PathnameStruct currentPathname = PathnameStruct.fromDesignator(pathname);
				InternalLoad.load(
						currentPathname,
						NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE,
						CommonLispSymbols.DEFAULT_KEYWORD
				);
			}
		}

		provide(moduleName);
		return TStruct.INSTANCE;
	}

	private static boolean moduleAlreadyExists(final StringStruct moduleName) {
		// TODO: Thread Safety issue here with global variable *modules*

		final ListStruct currentModulesList = CommonLispSymbols.MODULES_VAR.getVariableValue();

		for (final LispStruct module : currentModulesList) {
			if (module instanceof StringStruct) {
				final StringStruct currentModule = (StringStruct) module;
				if (currentModule.stringEqual(moduleName, null, null, null, null).toJavaPBoolean()) {
					return true;
				}
			}
		}

		return false;
	}
}
