package jcl.functions.list;

import java.util.ArrayList;
import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.ConsStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.parameterdsl.Parameters;

abstract class ConsMappingFunction extends CommonLispBuiltInFunctionStructBase {

	static final String LIST_ACCUMULATE = "LIST";
	static final String NCONC_ACCUMULATE = "NCONC";

	protected ConsMappingFunction(final String documentation, final String functionName,
	                              final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	LispStruct map1(final FunctionStruct function, final ListStruct originalArglists,
	                          final String accumulate, final boolean takeCar) {
		final ListStruct arglists = originalArglists.copyList();
		final ListStruct retList = ListStruct.toLispList(NILStruct.INSTANCE);
		ListStruct temp = retList;

		// Outer 'DO' vars
		ListStruct args = NILStruct.INSTANCE;

		while (true) {
			for (final LispStruct x : arglists) {
				if (NILStruct.INSTANCE.eq(x)) {
					if (accumulate != null) {
						return retList.getCdr();
					} else {
						return originalArglists.getCar();
					}
				}
			}

			// Inner 'DO' vars
			ListStruct l = arglists;

			while (!NILStruct.INSTANCE.eq(l)) {

				// PUSH
				final LispStruct carToPush;
				if (takeCar) {
					carToPush = ((ListStruct) l.getCar()).getCar();
				} else {
					carToPush = l.getCar();
				}
				args = ConsStruct.toLispCons(carToPush, args);

				// SETF
				final LispStruct cdarL = ((ListStruct) l.getCar()).getCdr();
				l.setCar(cdarL);

				// Update l Inner 'DO' var
				l = (ListStruct) l.getCdr();
			}

			// SETQ
			final LispStruct res = function.apply(args.nReverse().toArray());

			// CASE
			switch (accumulate) {
				case NCONC_ACCUMULATE:
					final List<ListStruct> lists = new ArrayList<>();
					for (final LispStruct curr : temp) {
						lists.add((ListStruct) curr);
					}
					final ListStruct nconc = (ListStruct) ListStruct.nConc(lists, res);
					temp = (ListStruct) nconc.last();
					break;
				case LIST_ACCUMULATE:
					temp.setCdr(ListStruct.toLispList(res));
					temp = (ListStruct) temp.getCdr();
					break;
			}

			// Update Args Outer 'DO' var
			args = NILStruct.INSTANCE;
		}
	}
}
