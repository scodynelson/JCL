/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class NthFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator typeValidator;

	public NthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDEX").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final IntegerStruct index
				= typeValidator.validateType(lispStructs[0], functionName(), "Index", IntegerType.INSTANCE, IntegerStruct.class);
		final ListStruct list
				= typeValidator.validateType(lispStructs[1], functionName(), "List", ListType.INSTANCE, ListStruct.class);

		final long indexValue = index.getBigInteger().longValue();
		return list.nth(indexValue);
	}

	@Override
	protected String functionName() {
		return "NTH";
	}
}
