/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PrintObjectFunction extends FunctionStruct {

	public static final SymbolStruct<?> PRINT_OBJECT = new SymbolStruct<>("PRINT-OBJECT", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -3100296760084297420L;

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintObjectFunction.class);

	@Autowired
	private Printer printer;

	private PrintObjectFunction() {
		super("Prints the provided object.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PRINT_OBJECT.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = new SymbolStruct<>("OBJECT", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation listArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(listArgSymbol, listArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return printObject(lispStructs[0]);
	}

	public LispStruct printObject(final LispStruct object) {
		final String printedObject = printer.print(object);
		LOGGER.info(printedObject);
		return object;
	}
}
