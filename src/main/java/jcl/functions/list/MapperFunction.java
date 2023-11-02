package jcl.functions.list;

import java.util.List;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.ConsStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

abstract class MapperFunction extends BuiltInFunctionStructImpl {

	private static final String FUNCTION_ARGUMENT = "FUNCTION";

	protected MapperFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter(FUNCTION_ARGUMENT)
		                .restParameter()
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FunctionStruct arg = arguments.getRequiredArgument(FUNCTION_ARGUMENT, FunctionStruct.class);
		final List<LispStruct> objects = arguments.getRestArgument();
		return listMapper(arg, ListStruct.toLispList(objects));
	}

	protected abstract Accumulate accumulate();

	protected abstract boolean takeCar();

	protected enum Accumulate {
		NONE,
		LIST,
		NCONC
	}

	private LispStruct listMapper(final FunctionStruct function, final ListStruct originalArglists) {
		final ListStruct arglists = originalArglists.copyList();
		final ConsStruct retList = ConsStruct.toLispCons(NILStruct.INSTANCE, NILStruct.INSTANCE);
		ListStruct temp = retList;

		// Outer 'DO' vars
		ListStruct args = NILStruct.INSTANCE;

		while (true) {
			for (final LispStruct arglist : arglists) {
				if (NILStruct.INSTANCE.eq(arglist)) {
					if (Accumulate.NONE != accumulate()) {
						return retList.cdr();
					} else {
						return originalArglists.car();
					}
				}
			}

			// Inner 'DO' vars
			ListStruct remainingLists = arglists;

			while (!NILStruct.INSTANCE.eq(remainingLists)) {

				// PUSH
				final LispStruct carToPush;
				if (takeCar()) {
					carToPush = ((ListStruct) remainingLists.car()).car();
				} else {
					carToPush = remainingLists.car();
				}
				args = ConsStruct.toLispCons(carToPush, args);

				// SETF
				final LispStruct cdar = ((ListStruct) remainingLists.car()).cdr();
				((ConsStruct) remainingLists).rplaca(cdar);

				// Update remainingLists Inner 'DO' var
				remainingLists = (ListStruct) remainingLists.cdr();
			}

			// SETQ
			LispStruct res = function.apply(args.nReverse().toArray());
			if (res instanceof ValuesStruct) {
				res = ((ValuesStruct) res).getPrimaryValue();
			}

			// CASE
			switch (accumulate()) {
				case NCONC:
					final ListStruct nconc = (ListStruct) NconcFunction.apply(List.of(temp, res));
					temp = (ListStruct) nconc.last();
					break;
				case LIST:
					if (temp == NILStruct.INSTANCE) {
						throw new TypeErrorException("Cannot convert value '" + temp + "' to type 'CONS'");
					}
					((ConsStruct) temp).rplacd(ListStruct.toLispList(res));
					temp = (ConsStruct) temp.cdr();
					break;
				case NONE:
					break;
			}

			// Update Args Outer 'DO' var
			args = NILStruct.INSTANCE;
		}
	}
}
