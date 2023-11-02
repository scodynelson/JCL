/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.List;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

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
		return apply(restArgument);
	}

	public static LispStruct apply(final List<LispStruct> listArg) {
		LispStruct result = null;
		ConsStruct splice = null;
		final int limit = listArg.size() - 1;
		int i;
		for (i = 0; i < limit; i++) {
			LispStruct list = listArg.get(i);
			if (list == NILStruct.INSTANCE) {
				continue;
			}
			if (list instanceof ConsStruct) {
				if (splice != null) {
					splice.rplacd(list);
					splice = (ConsStruct) list;
				}
				while (list instanceof ConsStruct) {
					if (result == null) {
						result = list;
						splice = (ConsStruct) result;
					} else {
						splice = (ConsStruct) list;
					}
					list = splice.cdr();
				}
			} else {
				throw new TypeErrorException("Cannot convert value '" + list + "' to type 'LIST'");
			}
		}
		if (result == null) {
			return listArg.get(i);
		}
		splice.rplacd(listArg.get(i));
		return result;
	}
}
