/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.Optional;

import jcl.compiler.environment.Environment;
import jcl.lang.BooleanStruct;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class MacroExpand1Function extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MACROEXPAND-1";
	private static final String FORM_ARGUMENT = "FORM";
	private static final String ENVIRONMENT_ARGUMENT = "ENVIRONMENT";

	public MacroExpand1Function() {
		super("Expands form once.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FORM_ARGUMENT)
		                .optionalParameter(ENVIRONMENT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		Environment environment = Environment.NULL;
		if (arguments.hasOptionalArgument(ENVIRONMENT_ARGUMENT)) {
			environment = arguments.getOptionalArgument(ENVIRONMENT_ARGUMENT, Environment.class);
		}

		final MacroExpandResult macroExpandResult = macroExpand1(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return ValuesStruct.valueOf(expandedForm, wasExpandedBoolean);
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

		final LispStruct first = form.getCar();
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
