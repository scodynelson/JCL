/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StringInputStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringInputStream extends FunctionStruct {

	public static final SymbolStruct MAKE_STRING_INPUT_STREAM = GlobalPackageStruct.COMMON_LISP.intern("MAKE-STRING-INPUT-STREAM").getSymbol();

	private static final long serialVersionUID = -7551808205259177133L;

	private MakeStringInputStream() {
		super("Returns an input string stream.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAKE_STRING_INPUT_STREAM.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MAKE_STRING_INPUT_STREAM);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct stringArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("STRING").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(stringArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final StringStruct stringStruct = (StringStruct) lispStructs[0];
		return makeStringInputStream(stringStruct.getAsJavaString());
	}

	public StringInputStreamStruct makeStringInputStream(final String stringValue) {
		return new StringInputStreamStruct(stringValue);
	}
}
