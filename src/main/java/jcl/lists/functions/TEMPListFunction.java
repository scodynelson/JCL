/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.FunctionParams;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

//@Component
public final class TEMPListFunction extends BuiltInFunctionStruct<TEMPListFunction.ListParams> {

	public static final SymbolStruct LIST = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();

	public TEMPListFunction() {
		super("Returns a list containing the supplied objects.", "LIST");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECTS").build();
	}

	@Override
	protected LispStruct internalApply(final ListParams params) {
		final List<LispStruct> objects = params.getObjects();
		return ListStruct.buildProperList(objects);
	}

	@Override
	protected ListParams getParams(final OrdinaryLambdaList lambdaList) {
		final RestParameter restParameter = lambdaList.getRestBinding();
		return new ListParams((List<LispStruct>) restParameter.getInitForm());
	}

	static class ListParams implements FunctionParams {

		private final List<LispStruct> objects;

		private ListParams(final List<LispStruct> objects) {
			this.objects = objects;
		}

		private List<LispStruct> getObjects() {
			return objects;
		}
	}

}
