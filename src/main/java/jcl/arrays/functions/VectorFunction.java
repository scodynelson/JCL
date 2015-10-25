/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

import java.util.Arrays;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RestParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class VectorFunction extends FunctionStruct {

	public static final SymbolStruct<?> VECTOR = GlobalPackageStruct.COMMON_LISP.intern("VECTOR").getSymbol();

	private static final long serialVersionUID = -2957696649653550853L;

	private VectorFunction() {
		super("Creates a fresh simple general vector whose size corresponds to the number of objects.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		VECTOR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(VECTOR);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> objectRestArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECTS").getSymbol();
		final RestParameter restBinding = new RestParameter(objectRestArgSymbol);

		return new OrdinaryLambdaList.Builder().restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return vector(lispStructs);
	}

	public LispStruct vector(final LispStruct... lispStructs) {
		return new VectorStruct<>(Arrays.asList(lispStructs));
	}
}
