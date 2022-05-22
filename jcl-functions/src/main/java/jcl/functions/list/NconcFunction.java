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

public final class NconcFunction extends BuiltInFunctionStructImpl {

	public NconcFunction() {
		super("Returns a list that is the concatenation of lists.",
		      CommonLispSymbols.NCONC.getName(),
		      Parameters.forFunction(CommonLispSymbols.NCONC.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.NCONC;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> restArgument = arguments.getRestArgument();
		return nConc(restArgument);
	}

	static LispStruct nConc(final List<LispStruct> listArg) {
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
		return nConc(lists, object);
	}

	static LispStruct nConc(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.eq(list)) {
				continue;
			}

			if (NILStruct.INSTANCE.eq(result)) {
				result = list;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ConsStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ConsStruct lastOfResult = (ConsStruct) last;
			lastOfResult.rplacd(list);
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
