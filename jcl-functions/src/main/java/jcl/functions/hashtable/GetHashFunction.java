/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class GetHashFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GETHASH";
	private static final String KEY_ARGUMENT = "KEY";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";
	private static final String DEFAULT_ARGUMENT = "DEFAULT";

	public GetHashFunction() {
		super("Retrieve the value in the hash-table whose key is the same as the key under the hash-table's equivalence test. If there is no such entry, the result is the default.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(KEY_ARGUMENT)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		                .optionalParameter(DEFAULT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct key = arguments.getRequiredArgument(KEY_ARGUMENT);
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);
		final LispStruct defaultVal = arguments.getOptionalArgument(DEFAULT_ARGUMENT);

		final LispStruct hash = hashTable.getHash(key);
		return (hash == null) ? defaultVal : hash;
	}
}
