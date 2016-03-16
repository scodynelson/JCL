/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.parameterdsl.FunctionParameters;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.util.ClassUtils;

//@Component
public final class TEMPAppendFunction extends BuiltInFunctionStruct {

	public TEMPAppendFunction() {
		super("Returns a new list that is the concatenation of the copies.", "APPEND");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "LISTS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final int length = lispStructs.length;
		if (length == 0) {
			return NILStruct.INSTANCE;
		}
		if (length == 1) {
			return lispStructs[0];
		}

		final Iterator<LispStruct> iterator = Arrays.asList(lispStructs).iterator();

		final List<ListStruct> lists = new ArrayList<>(lispStructs.length - 1);
		LispStruct object = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final LispStruct next = iterator.next();
			if (!iterator.hasNext()) {
				object = next;
				break;
			}

			final ListStruct list = ClassUtils.convert(ListStruct.class, next);
			lists.add(list);
		}

		return ListStruct.append(lists, object);
	}

	@Override
	public LispStruct apply(final FunctionParameters params) {
//		final List<ListStruct> lists = params.getLists();
//		final LispStruct object = params.getObject();
//		return ListStruct.append(lists, object);
		return null;
	}

	@Override
	protected FunctionParameters getParams(final List<LispStruct> lispStructs) {
//		final RestParameter restParameter = lambdaList.getRestBinding();
//		final List<ListStruct> initForm = (List<ListStruct>) restParameter.getInitForm();

//		final IntegerStruct convertInitialElement = ClassUtils.convert(restParameter.getInitFormClass(), restParameter.getInitForm());

		return null;
	}
}
