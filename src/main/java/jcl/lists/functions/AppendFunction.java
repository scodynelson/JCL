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
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class AppendFunction extends AbstractCommonLispFunctionStruct {

	public static final SymbolStruct APPEND = GlobalPackageStruct.COMMON_LISP.intern("APPEND").getSymbol();

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public AppendFunction() {
		super("Returns a new list that is the concatenation of the copies.");
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

			final ListStruct list
					= validator.validateType(next, functionName(), "List", ListType.INSTANCE, ListStruct.class);
			lists.add(list);
		}

		return ListStruct.append(lists, object);
	}

	@Override
	protected String functionName() {
		return "APPEND";
	}
}
