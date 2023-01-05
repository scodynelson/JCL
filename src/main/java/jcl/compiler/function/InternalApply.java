package jcl.compiler.function;

import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class InternalApply {

	public LispStruct apply(final LispStruct functionDesignator,
	                        final LispStruct arg,
	                        final ListStruct restArgs) {

		FunctionStruct functionStruct = null;
		if (functionDesignator instanceof SymbolStruct) {
			functionStruct = ((SymbolStruct) functionDesignator).symbolFunction();
		} else if (functionDesignator instanceof FunctionStruct) {
			functionStruct = (FunctionStruct) functionDesignator;
		}

		final List<LispStruct> args = restArgs.toJavaList();

		if (functionStruct == null) {
//			final String printedArguments = printer.print(functionList);
			final String printedArguments = arg + " " + args;
			final String message = "Undefined function " + functionDesignator + " called with arguments " + printedArguments;
			throw new ErrorException(message);
		}

		if (args.isEmpty()) {
			if (!(arg instanceof final ListStruct argAsList)) {
				throw new ErrorException("Can't construct argument list from " + arg + '.');
			}

			final LispStruct[] lispStructs = argAsList.toArray();
			return functionStruct.apply(lispStructs);
		}
		final int argsSize = args.size();

		final LispStruct lastElement = args.get(argsSize - 1);
		if (!(lastElement instanceof final ListStruct lastElementList)) {
			throw new ErrorException("Can't construct argument list from " + lastElement + '.');
		}
		final LispStruct[] lastElementArray = lastElementList.toArray();
		final int lastElementSize = lastElementArray.length;

		// NOTE: We add 1 for the required argument, but also subtract 1 for the last element being a list
		final LispStruct[] argumentsArrays = new LispStruct[argsSize + lastElementSize];
		argumentsArrays[0] = arg;
		for (int i = 0; i < (argsSize - 1); i++) {
			argumentsArrays[i + 1] = args.get(i);
		}
		System.arraycopy(lastElementArray, 0, argumentsArrays, argsSize, lastElementSize);

		return functionStruct.apply(argumentsArrays);
	}

	public static LispStruct funcall(final LispStruct functionDesignator,
	                                 final ListStruct restArgs) {
		return apply(functionDesignator, restArgs, NILStruct.INSTANCE);
	}
}
