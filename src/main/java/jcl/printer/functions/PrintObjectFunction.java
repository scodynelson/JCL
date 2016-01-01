/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
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

	public static final SymbolStruct PRINT_OBJECT = GlobalPackageStruct.COMMON_LISP.intern("PRINT-OBJECT").getSymbol();

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
		GlobalPackageStruct.COMMON_LISP.export(PRINT_OBJECT);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
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
