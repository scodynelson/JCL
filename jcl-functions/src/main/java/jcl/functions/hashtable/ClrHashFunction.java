/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ClrHashFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "CLRHASH";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";

	public ClrHashFunction() {
		super("Removes all entries from hash-table, and then returns that empty hash table.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);
		hashTable.clrHash();
		return hashTable;
	}
}
