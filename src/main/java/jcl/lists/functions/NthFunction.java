/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.ArrayList;
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

	private static final long serialVersionUID = 2998151869440671653L;

	@Autowired
	private TypeValidator typeValidator;

	public NthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final RequiredParameter object1RequiredBinding = new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "INDEX").build();
		requiredBindings.add(object1RequiredBinding);

		final RequiredParameter object2RequiredBinding = new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "LIST").build();
		requiredBindings.add(object2RequiredBinding);

		return requiredBindings;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct indexArg = lispStructs[0];
		typeValidator.validateTypes(indexArg, functionName(), "Index", IntegerType.INSTANCE);
		final IntegerStruct index = (IntegerStruct) indexArg;

		final LispStruct listArg = lispStructs[1];
		typeValidator.validateTypes(indexArg, functionName(), "List", ListType.INSTANCE);
		final ListStruct list = (ListStruct) listArg;

		final int indexValue = index.getBigInteger().intValue();
		return list.getElement(indexValue);
	}

	@Override
	protected String functionName() {
		return "NTH";
	}
}
