/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.io.Serializable;
import java.util.Optional;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.environment.Environment;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class MacroExpandFunction implements Serializable {

	private static final long serialVersionUID = 5991270831364188635L;

	public MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		MacroExpandResult expansion;

		if (element instanceof ListStruct) {
			expansion = macroExpand1((ListStruct) element, environment);
		} else if (element instanceof SymbolStruct) {
			expansion = macroExpand1((SymbolStruct<?>) element, environment);
		} else {
			expansion = new MacroExpandResult(element, false);
		}

		LispStruct expandedForm = expansion.getExpandedForm();
		boolean wasExpanded = expansion.wasExpanded();

		while (wasExpanded) {
			expansion = macroExpand(expandedForm, environment);

			expandedForm = expansion.getExpandedForm();
			wasExpanded = expansion.wasExpanded();
		}

		return expansion;
	}

	// MacroExpand1

	public MacroExpandResult macroExpand1(final ListStruct form, final Environment environment) {

		final LispStruct first = form.getFirst();
		if (first instanceof SymbolStruct<?>) {

			final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct((SymbolStruct<?>) first);
			if (symbolStruct.isPresent()) {
				final SymbolStruct<?> theSymbol = symbolStruct.get();

				final MacroFunctionExpander<?> macroFunctionExpander = theSymbol.getMacroFunctionExpander();

				if (macroFunctionExpander != null) {
					final FunctionStruct macroExpandHook = CompilerVariables.MACROEXPAND_HOOK.getValue();
					final LispStruct expansion = macroExpandHook.apply(macroFunctionExpander, form, environment);

					return new MacroExpandResult(expansion, true);
				}
				// TODO: support compiler-macro-functions
			}
		}

		return new MacroExpandResult(form, false);
	}

	public MacroExpandResult macroExpand1(final SymbolStruct<?> form, final Environment environment) {

		final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct(form);
		if (symbolStruct.isPresent()) {
			final SymbolStruct<?> theSymbol = symbolStruct.get();

			final SymbolMacroExpander<?> symbolMacroExpander = theSymbol.getSymbolMacroExpander();

			if (symbolMacroExpander != null) {
				final FunctionStruct macroExpandHook = CompilerVariables.MACROEXPAND_HOOK.getValue();
				final LispStruct expansion = macroExpandHook.apply(symbolMacroExpander, form, environment);

				return new MacroExpandResult(expansion, true);
			}
		}

		return new MacroExpandResult(form, false);
	}

	private static Optional<SymbolStruct<?>> getSymbolStruct(final SymbolStruct<?> symbolElement) {
		final PackageStruct thePackage = symbolElement.getSymbolPackage();
		if (thePackage != null) {

			final String symbolName = symbolElement.getName();
			final PackageSymbolStruct thePackageSymbol = thePackage.findSymbol(symbolName);

			if (thePackageSymbol != null) {
				final SymbolStruct<?> theSymbol = thePackageSymbol.getSymbol();
				return Optional.ofNullable(theSymbol);
			}
		}

		return Optional.empty();
	}
}
