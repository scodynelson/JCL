/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import java.math.BigInteger;

import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class HashTableCountFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "HASH-TABLE-COUNT";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";

	public HashTableCountFunction() {
		super("Returns the number of entries in the hash-table.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);
		final BigInteger count = hashTable.getCount();
		return LispStructFactory.toInteger(count);
	}
}
