/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameDevice;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameDeviceFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME_DEVICE = new SymbolStruct<>("PATHNAME-DEVICE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 7115138569329848864L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameDeviceFunction() {
		super("Returns the pathname-device component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_DEVICE.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = new SymbolStruct<>("PATHSPEC", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation pathspecArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol, pathspecArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final SymbolStruct<?> caseArgSymbol = new SymbolStruct<>("CASE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation caseArgAllocation = new ParameterAllocation(1);

		final SymbolStruct<?> caseSuppliedPSymbol = new SymbolStruct<>("CASE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation suppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(caseSuppliedPSymbol, suppliedPAllocation);

		final KeyBinding keyBinding = new KeyBinding(caseArgSymbol, caseArgAllocation, NullStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyBinding> keyBindings = Collections.singletonList(keyBinding);

		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		final PathnameDevice pathnameDevice = pathnameDevice(pathspec);
		if (pathnameDevice == null) {
			return NullStruct.INSTANCE;
		}

		final String device = pathnameDevice.getDevice();
		final LispStruct returnValue;

		if (device == null) {
			final PathnameComponentType componentType = pathnameDevice.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = new StringStruct(device);
		}

		return returnValue;
	}

	public PathnameDevice pathnameDevice(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameDevice();
	}
}
