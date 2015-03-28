/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroExpandFunction extends FunctionStruct {

	public static final SymbolStruct<?> MACROEXPAND = new SymbolStruct<>("MACROEXPAND", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 5991270831364188635L;

	@Autowired
	private MacroExpand1Function macroExpand1Function;

	private MacroExpandFunction() {
		super("Repeatedly expands form until it is no longer a macro form.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MACROEXPAND.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> formArgSymbol = new SymbolStruct<>("FORM", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation formArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(formArgSymbol, formArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> envArgSymbol = new SymbolStruct<>("ENV", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation evnArgAllocation = new ParameterAllocation(1);

		final SymbolStruct<?> envSuppliedPSymbol = new SymbolStruct<>("ENV-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation suppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(envSuppliedPSymbol, suppliedPAllocation);

		final OptionalBinding optionalBinding = new OptionalBinding(envArgSymbol, evnArgAllocation, NullStruct.INSTANCE, suppliedPBinding);
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

		final MacroExpandResult macroExpandResult = macroExpand(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return new ValuesStruct(expandedForm, wasExpandedBoolean);
	}

	public MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		MacroExpandResult expansion = macroExpand1Function.macroExpand1(element, environment);

		LispStruct expandedForm = expansion.getExpandedForm();
		boolean wasExpanded = expansion.wasExpanded();

		while (wasExpanded) {
			expansion = macroExpand(expandedForm, environment);

			expandedForm = expansion.getExpandedForm();
			wasExpanded = expansion.wasExpanded();
		}

		return expansion;
	}
}
