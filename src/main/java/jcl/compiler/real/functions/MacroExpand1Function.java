/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
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

	public static final SymbolStruct<?> MACROEXPAND_1 = new SymbolStruct<>("MACROEXPAND-1", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 5991270831364188635L;

	private MacroExpand1Function() {
		super("Expands form once.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MACROEXPAND_1.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> formArgSymbol = new SymbolStruct<>("FORM", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding requiredBinding = new RequiredBinding(formArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> envArgSymbol = new SymbolStruct<>("ENV", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> envSuppliedPSymbol = new SymbolStruct<>("ENV-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(envSuppliedPSymbol);

		final OptionalBinding optionalBinding = new OptionalBinding(envArgSymbol, NullStruct.INSTANCE, suppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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
			return macroExpand1((SymbolStruct<?>) element, environment);
		} else {
			return new MacroExpandResult(element, false);
		}
	}

	private static MacroExpandResult macroExpand1(final ListStruct form, final Environment environment) {

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

	private static MacroExpandResult macroExpand1(final SymbolStruct<?> form, final Environment environment) {

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
