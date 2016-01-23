/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.struct.ValuesStruct;
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
public final class MacroExpandFunction extends FunctionStruct {

	public static final SymbolStruct MACROEXPAND = GlobalPackageStruct.COMMON_LISP.intern("MACROEXPAND").getSymbol();

	@Autowired
	private MacroExpand1Function macroExpand1Function;

	private MacroExpandFunction() {
		super("Repeatedly expands form until it is no longer a macro form.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MACROEXPAND.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MACROEXPAND);
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

		final MacroExpandResult macroExpandResult = macroExpand(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return new ValuesStruct(expandedForm, wasExpandedBoolean);
	}

	public MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		LispStruct tempElement = element;

		boolean wasExpanded = false;
		while (true) {
			final MacroExpandResult expansion = macroExpand1Function.macroExpand1(tempElement, environment);
			tempElement = expansion.getExpandedForm();

			final boolean innerWasNotExpanded = !expansion.wasExpanded();
			if (innerWasNotExpanded) {
				return new MacroExpandResult(tempElement, wasExpanded);
			}
			wasExpanded = true;
		}
	}
}
