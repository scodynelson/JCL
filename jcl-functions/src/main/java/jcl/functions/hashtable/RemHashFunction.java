/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.hashtable.HashTableStruct;
import org.springframework.stereotype.Component;

@Component
public final class RemHashFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "REMHASH";
	private static final String KEY_ARGUMENT = "KEY";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";

	public RemHashFunction() {
		super("Removes the entry for key in hash-table, if any. Returns true if there was such an entry, or false otherwise.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(KEY_ARGUMENT)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct key = arguments.getRequiredArgument(KEY_ARGUMENT);
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);

		final LispStruct removedValue = hashTable.remHash(key);
		return BooleanStruct.toLispBoolean(removedValue == null);
	}
}
