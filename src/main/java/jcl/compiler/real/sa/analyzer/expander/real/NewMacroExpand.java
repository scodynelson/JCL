/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import java.util.Optional;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class NewMacroExpand {

	public NewMacroExpandReturn macroExpand(final LispStruct element, final AnalysisBuilder analysisBuilder) {
		NewMacroExpandReturn expansion;

		if (element instanceof ListStruct) {
			expansion = macroExpand1((ListStruct) element, analysisBuilder);
		} else if (element instanceof SymbolStruct) {
			expansion = macroExpand1((SymbolStruct<?>) element, analysisBuilder);
		} else {
			expansion = new NewMacroExpandReturn(element, false);
		}

		LispStruct expandedForm = expansion.getExpandedForm();
		boolean wasExpanded = expansion.wasExpanded();

		while (wasExpanded) {
			expansion = macroExpand(expandedForm, analysisBuilder);

			expandedForm = expansion.getExpandedForm();
			wasExpanded = expansion.wasExpanded();
		}

		return expansion;
	}

	// MacroExpand1

	public NewMacroExpandReturn macroExpand1(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final LispStruct first = form.getFirst();
		if (first instanceof SymbolStruct<?>) {

			final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct((SymbolStruct<?>) first);
			if (symbolStruct.isPresent()) {
				final SymbolStruct<?> theSymbol = symbolStruct.get();

				final MacroFunctionExpander macroFunctionExpander = theSymbol.getMacroFunctionExpander();

				if (macroFunctionExpander != null) {
					final LispStruct expansion = macroFunctionExpander.expand(form, analysisBuilder);
					return new NewMacroExpandReturn(expansion, true);
				}
				// TODO: support compiler-macro-functions
			}
		}

		return new NewMacroExpandReturn(form, false);
	}

	public NewMacroExpandReturn macroExpand1(final SymbolStruct<?> form, final AnalysisBuilder analysisBuilder) {

		final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct(form);
		if (symbolStruct.isPresent()) {
			final SymbolStruct<?> theSymbol = symbolStruct.get();

			final SymbolMacroExpander symbolMacroExpander = theSymbol.getSymbolMacroExpander();

			if (symbolMacroExpander != null) {
				final LispStruct expansion = symbolMacroExpander.expand(form, analysisBuilder);
				return new NewMacroExpandReturn(expansion, true);
			}
		}

		return new NewMacroExpandReturn(form, false);
	}

	private Optional<SymbolStruct<?>> getSymbolStruct(final SymbolStruct<?> symbolElement) {
		final PackageStruct thePackage = symbolElement.getSymbolPackage();
		if (thePackage != null) {

			final String symbolName = symbolElement.getName();
			final PackageSymbolStruct thePackageSymbol = thePackage.findSymbol(symbolName);

			if (thePackageSymbol != null) {
				final SymbolStruct<?> theSymbol = thePackageSymbol.getSymbolStruct();
				return Optional.ofNullable(theSymbol);
			}
		}

		return Optional.empty();
	}
}
