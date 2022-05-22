/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import org.apache.commons.collections4.iterators.ReverseListIterator;

public final class AppendFunction extends BuiltInFunctionStructImpl {

	public AppendFunction() {
		super("Returns a new list that is the concatenation of the copies.",
		      CommonLispSymbols.APPEND.getName(),
		      Parameters.forFunction(CommonLispSymbols.APPEND.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.APPEND;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> restArgument = arguments.getRestArgument();
		return append(restArgument);
	}

	private static LispStruct append(final List<LispStruct> listArg) {
		final int size = listArg.size();
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return listArg.get(0);
		}

		final ReverseListIterator<LispStruct> reverseListIterator = new ReverseListIterator<>(listArg);
		final LispStruct object = reverseListIterator.next();

		final List<ListStruct> lists = new ArrayList<>();
		reverseListIterator.forEachRemaining(argument -> {
			if (argument instanceof ListStruct) {
				lists.add((ListStruct) argument);
			} else {
				throw new TypeErrorException("Cannot convert value '" + argument + "' to type 'LIST'");
			}
		});
		return append(lists, object);
	}

	private static LispStruct append(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.eq(list)) {
				continue;
			}

			final ListStruct copyList = list.copyList();
			if (NILStruct.INSTANCE.eq(result)) {
				result = copyList;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ConsStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ConsStruct lastOfResult = (ConsStruct) last;
			lastOfResult.rplacd(copyList);
		}

		if (NILStruct.INSTANCE.eq(result)) {
			return object;
		}

		final LispStruct last = result.last();
		if (!(last instanceof ConsStruct)) {
			throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
		}
		final ConsStruct lastOfResult = (ConsStruct) last;
		lastOfResult.rplacd(object);

		return result;
	}
}
