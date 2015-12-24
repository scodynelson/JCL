/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
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

	public static final SymbolStruct PATHNAME_DEVICE = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME-DEVICE").getSymbol();

	private static final long serialVersionUID = 7115138569329848864L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameDeviceFunction() {
		super("Returns the pathname-device component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_DEVICE.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME_DEVICE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct caseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE").getSymbol();

		final SymbolStruct caseSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE-P-").getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(caseSuppliedPSymbol);

		final KeyParameter keyBinding = new KeyParameter(caseArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyParameter> keyBindings = Collections.singletonList(keyBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .keyBindings(keyBindings)
		                                               .build();
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
