/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.FunctionParams;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

//@Component
public final class TEMPConsFunction extends BuiltInFunctionStruct<TEMPConsFunction.ConsParams> {

	public static final SymbolStruct CONS = GlobalPackageStruct.COMMON_LISP.intern("CONS").getSymbol();

	public TEMPConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.", "CONS");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT-1").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT-2").build()
		);
	}

	@Override
	protected LispStruct internalApply(final ConsParams params) {
		final LispStruct object1 = params.getObject1();
		final LispStruct object2 = params.getObject2();
		return new ConsStruct(object1, object2);
	}

	@Override
	protected ConsParams getParams(final OrdinaryLambdaList lambdaList) {
		final List<RequiredParameter> requiredBindings = lambdaList.getRequiredBindings();
		return new ConsParams(requiredBindings.get(0), requiredBindings.get(1));
	}

	static final class ConsParams implements FunctionParams {

		private final RequiredParameter object1;
		private final RequiredParameter object2;

		private ConsParams(final RequiredParameter object1, final RequiredParameter object2) {
			this.object1 = object1;
			this.object2 = object2;
		}

		private LispStruct getObject1() {
			return object1.getInitForm();
		}

		private LispStruct getObject2() {
			return object2.getInitForm();
		}
	}
}
