/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.array;

import java.util.List;
import java.util.stream.Collectors;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.VectorStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ListToVectorFunction extends BuiltInFunctionStructImpl {

	private static final String LIST_ARGUMENT = "LIST";

	public ListToVectorFunction() {
		super("Creates a fresh simple general vector from the provided list.",
		      CommonLispSymbols.LIST_TO_VECTOR.getName(),
		      Parameters.forFunction(CommonLispSymbols.LIST_TO_VECTOR.getName())
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LIST_TO_VECTOR;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final List<LispStruct> contents = list.stream().collect(Collectors.toList());
		final IntegerStruct size = IntegerStruct.toLispInteger(contents.size());
		return VectorStruct.toLispVector(size, CommonLispSymbols.T, contents);
	}
}
