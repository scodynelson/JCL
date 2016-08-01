/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.ArrayList;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.util.ClassUtils;
import org.apache.commons.collections4.iterators.ReverseListIterator;
import org.springframework.stereotype.Component;

@Component
public final class AppendFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "APPEND";

	public AppendFunction() {
		super("Returns a new list that is the concatenation of the copies.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> restArgument = arguments.getRestArgument();

		final int size = restArgument.size();
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return restArgument.get(0);
		}

		final ReverseListIterator<LispStruct> reverseListIterator = new ReverseListIterator<>(restArgument);
		final LispStruct object = reverseListIterator.next();

		final List<ListStruct> lists = new ArrayList<>();
		reverseListIterator.forEachRemaining(argument -> {
			final ListStruct list = ClassUtils.convert(argument, ListStruct.class);
			lists.add(list);
		});
		return ListStruct.append(lists, object);
	}
}
