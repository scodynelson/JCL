package jcl.compiler.function;

import java.util.Optional;

import jcl.compiler.environment.Environment;
import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class InternalMacroExpand {

	public static MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		LispStruct tempElement = element;

		boolean wasExpanded = false;
		while (true) {
			final MacroExpandResult expansion = macroExpand1(tempElement, environment);
			tempElement = expansion.getExpandedForm();

			final boolean innerWasNotExpanded = !expansion.getWasExpanded().toJavaPBoolean();
			if (innerWasNotExpanded) {
				return new MacroExpandResult(tempElement, BooleanStruct.toLispBoolean(wasExpanded));
			}
			wasExpanded = true;
		}
	}

	public static MacroExpandResult macroExpand1(final LispStruct element, final Environment environment) {
		if (element instanceof ListStruct) {
			return macroExpand1((ListStruct) element, environment);
		} else if (element instanceof SymbolStruct) {
			return macroExpand1((SymbolStruct) element, environment);
		} else {
			return new MacroExpandResult(element, NILStruct.INSTANCE);
		}
	}

	private static MacroExpandResult macroExpand1(final ListStruct form, final Environment environment) {

		final LispStruct first = form.car();
		if (first instanceof SymbolStruct) {

			final Optional<SymbolStruct> symbolStruct = getSymbolStruct((SymbolStruct) first);
			if (symbolStruct.isPresent()) {
				final SymbolStruct theSymbol = symbolStruct.get();

				final MacroFunctionExpanderInter macroFunctionExpander = theSymbol.getMacroFunctionExpander();

				if (macroFunctionExpander != null) {
					final FunctionStruct macroExpandHook = CommonLispSymbols.MACROEXPAND_HOOK_VAR.getVariableValue();
					final LispStruct expansion = macroExpandHook.apply(macroFunctionExpander, form, environment);

					return new MacroExpandResult(expansion, TStruct.INSTANCE);
				}
				// TODO: support compiler-macro-functions
			}
		}

		return new MacroExpandResult(form, NILStruct.INSTANCE);
	}

	private static MacroExpandResult macroExpand1(final SymbolStruct form, final Environment environment) {

		final Optional<SymbolStruct> symbolStruct = getSymbolStruct(form);
		if (symbolStruct.isPresent()) {
			final SymbolStruct theSymbol = symbolStruct.get();

			final SymbolMacroExpanderInter symbolMacroExpander = theSymbol.getSymbolMacroExpander();

			if (symbolMacroExpander != null) {
				final FunctionStruct macroExpandHook = CommonLispSymbols.MACROEXPAND_HOOK_VAR.getVariableValue();
				final LispStruct expansion = macroExpandHook.apply(symbolMacroExpander, form, environment);

				return new MacroExpandResult(expansion, TStruct.INSTANCE);
			}
		}

		return new MacroExpandResult(form, NILStruct.INSTANCE);
	}

	private static Optional<SymbolStruct> getSymbolStruct(final SymbolStruct symbolElement) {
		final PackageStruct thePackage = symbolElement.getSymbolPackage();
		if (thePackage != null) {

			final String symbolName = symbolElement.getName();
			final PackageSymbolStruct thePackageSymbol = thePackage.findSymbol(symbolName);

			if (thePackageSymbol.found()) {
				final SymbolStruct theSymbol = thePackageSymbol.getSymbol();
				return Optional.ofNullable(theSymbol);
			}
		}

		return Optional.empty();
	}
}
