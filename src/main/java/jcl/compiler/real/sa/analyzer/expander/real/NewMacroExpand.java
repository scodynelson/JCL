/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class NewMacroExpand {

	public NewMacroExpandReturn macroExpand(final Element element, final AnalysisBuilder analysisBuilder) {
		NewMacroExpandReturn expansion;

		if (element instanceof ConsElement) {
			expansion = macroExpand1((ConsElement) element, analysisBuilder);
		} else if (element instanceof SymbolElement) {
			expansion = macroExpand1((SymbolElement) element, analysisBuilder);
		} else {
			expansion = new NewMacroExpandReturn(element, false);
		}

		Element expandedForm = expansion.getExpandedForm();
		boolean wasExpanded = expansion.wasExpanded();

		while (wasExpanded) {
			expansion = macroExpand(expandedForm, analysisBuilder);

			expandedForm = expansion.getExpandedForm();
			wasExpanded = expansion.wasExpanded();
		}

		return expansion;
	}

	// MacroExpand1

	public NewMacroExpandReturn macroExpand1(final ConsElement form, final AnalysisBuilder analysisBuilder) {

		final SimpleElement first = form.getElements().getFirst();
		if (first instanceof SymbolElement) {

			final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct((SymbolElement) first);
			if (symbolStruct.isPresent()) {
				final SymbolStruct<?> theSymbol = symbolStruct.get();

				final MacroFunctionExpander macroFunctionExpander = theSymbol.getMacroFunctionExpander();

				if (macroFunctionExpander != null) {
					final Element expansion = macroFunctionExpander.expand(form, analysisBuilder);
					return new NewMacroExpandReturn(expansion, true);
				}
				// TODO: support compiler-macro-functions
			}
		}

		return new NewMacroExpandReturn(form, false);
	}

	public NewMacroExpandReturn macroExpand1(final SymbolElement form, final AnalysisBuilder analysisBuilder) {

		final Optional<SymbolStruct<?>> symbolStruct = getSymbolStruct(form);
		if (symbolStruct.isPresent()) {
			final SymbolStruct<?> theSymbol = symbolStruct.get();

			final SymbolMacroExpander symbolMacroExpander = theSymbol.getSymbolMacroExpander();

			if (symbolMacroExpander != null) {
				final Element expansion = symbolMacroExpander.expand(form, analysisBuilder);
				return new NewMacroExpandReturn(expansion, true);
			}
		}

		return new NewMacroExpandReturn(form, false);
	}

	private Optional<SymbolStruct<?>> getSymbolStruct(final SymbolElement symbolElement) {
		final String packageName = symbolElement.getPackageName();
		if (packageName != null) {

			final PackageStruct thePackage = PackageStruct.findPackage(packageName);
			if (thePackage != null) {

				final String symbolName = symbolElement.getSymbolName();
				final PackageSymbolStruct thePackageSymbol = thePackage.findSymbol(symbolName);

				final SymbolStruct<?> theSymbol = thePackageSymbol.getSymbolStruct();
				return Optional.ofNullable(theSymbol);
			}
		}

		return Optional.empty();
	}
}
