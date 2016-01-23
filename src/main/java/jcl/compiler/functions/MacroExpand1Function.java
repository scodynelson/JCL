/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.CompilerVariables;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class MacroExpand1Function extends FunctionStruct {

	public static final SymbolStruct MACROEXPAND_1 = GlobalPackageStruct.COMMON_LISP.intern("MACROEXPAND-1").getSymbol();

	private MacroExpand1Function() {
		super("Expands form once.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MACROEXPAND_1.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MACROEXPAND_1);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct formArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FORM").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(formArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct envArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("ENV").getSymbol();

		final SymbolStruct envSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("ENV-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(envSuppliedPSymbol);

		final OptionalParameter optionalBinding = new OptionalParameter(envArgSymbol, NullStruct.INSTANCE, suppliedPBinding);
		final List<OptionalParameter> optionalBindings = Collections.singletonList(optionalBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .optionalBindings(optionalBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct form = lispStructs[0];
		Environment environment = Environment.NULL;
		if (lispStructs.length == 2) {
			environment = (Environment) lispStructs[1];
		}

		final MacroExpandResult macroExpandResult = macroExpand1(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return new ValuesStruct(expandedForm, wasExpandedBoolean);
	}

	public MacroExpandResult macroExpand1(final LispStruct element, final Environment environment) {
		if (element instanceof ListStruct) {
			return macroExpand1((ListStruct) element, environment);
		} else if (element instanceof SymbolStruct) {
			return macroExpand1((SymbolStruct) element, environment);
		} else {
			return new MacroExpandResult(element, false);
		}
	}

	private static MacroExpandResult macroExpand1(final ListStruct form, final Environment environment) {

		final LispStruct first = form.getFirst();
		if (first instanceof SymbolStruct) {

			final Optional<SymbolStruct> symbolStruct = getSymbolStruct((SymbolStruct) first);
			if (symbolStruct.isPresent()) {
				final SymbolStruct theSymbol = symbolStruct.get();

				final MacroFunctionExpander<?> macroFunctionExpander = theSymbol.getMacroFunctionExpander();

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

			final SymbolMacroExpander<?> symbolMacroExpander = theSymbol.getSymbolMacroExpander();

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
