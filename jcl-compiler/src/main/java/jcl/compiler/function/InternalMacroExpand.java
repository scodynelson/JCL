package jcl.compiler.function;

import java.util.Optional;

import jcl.compiler.environment.Environment;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.CompilerVariables;

public class InternalMacroExpand {

	public static MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		LispStruct tempElement = element;

		boolean wasExpanded = false;
		while (true) {
			final MacroExpandResult expansion = macroExpand1(tempElement, environment);
			tempElement = expansion.getExpandedForm();

			final boolean innerWasNotExpanded = !expansion.wasExpanded();
			if (innerWasNotExpanded) {
				return new MacroExpandResult(tempElement, wasExpanded);
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
			return new MacroExpandResult(element, false);
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
					final FunctionStruct macroExpandHook = CompilerVariables.MACROEXPAND_HOOK.getVariableValue();
					final LispStruct expansion = macroExpandHook.apply(macroFunctionExpander, form, environment);

					return new MacroExpandResult(expansion, true);
				}
				// TODO: support compiler-macro-functions
			}
		}

		return new MacroExpandResult(form, false);
	}

	private static MacroExpandResult macroExpand1(final SymbolStruct form, final Environment environment) {

		final Optional<SymbolStruct> symbolStruct = getSymbolStruct(form);
		if (symbolStruct.isPresent()) {
			final SymbolStruct theSymbol = symbolStruct.get();

			final SymbolMacroExpanderInter symbolMacroExpander = theSymbol.getSymbolMacroExpander();

			if (symbolMacroExpander != null) {
				final FunctionStruct macroExpandHook = CompilerVariables.MACROEXPAND_HOOK.getVariableValue();
				final LispStruct expansion = macroExpandHook.apply(symbolMacroExpander, form, environment);

				return new MacroExpandResult(expansion, true);
			}
		}

		return new MacroExpandResult(form, false);
	}

	private static Optional<SymbolStruct> getSymbolStruct(final SymbolStruct symbolElement) {
		final PackageStruct thePackage = symbolElement.getSymbolPackage();
		if (thePackage != null) {

			final String symbolName = symbolElement.getName();
			final PackageSymbolStruct thePackageSymbol = thePackage.findSymbol(symbolName);

			if (thePackageSymbol != null) {
				final SymbolStruct theSymbol = thePackageSymbol.getSymbol();
				return Optional.ofNullable(theSymbol);
			}
		}

		return Optional.empty();
	}
}
