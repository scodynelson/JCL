/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class MapHashFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAPHASH";
	private static final String FUNCTION_ARGUMENT = "FUNCTION";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";

	public MapHashFunction() {
		super("Iterates over all entries in the hash-table. For each entry, the function is called with two arguments--the key and the value of that entry.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FUNCTION_ARGUMENT)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct function = arguments.getRequiredArgument(FUNCTION_ARGUMENT);
		final FunctionStruct functionVal = validateFunctionDesignator(function);
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);

		hashTable.mapHash(functionVal);
		return NILStruct.INSTANCE;
	}

	private FunctionStruct validateFunctionDesignator(final LispStruct functionDesignator) {
		if (functionDesignator instanceof FunctionStruct) {
			return (FunctionStruct) functionDesignator;
		} else if (functionDesignator instanceof SymbolStruct) {
			return ((SymbolStruct) functionDesignator).getFunction();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}
}
