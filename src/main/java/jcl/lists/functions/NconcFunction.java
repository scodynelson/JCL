/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.symbols.NILStruct;
import jcl.util.ClassUtils;
import org.apache.commons.collections4.iterators.ReverseListIterator;
import org.springframework.stereotype.Component;

@Component
public final class NconcFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "NCONC";

	public NconcFunction() {
		super("Returns a list that is the concatenation of lists.",
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
			final ListStruct list = ClassUtils.convert(ListStruct.class, argument);
			lists.add(list);
		});
		return ListStruct.nConc(lists, object);
	}
}
