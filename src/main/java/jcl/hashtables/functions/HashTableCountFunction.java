/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.hashtables.HashTableStruct;
import jcl.numbers.IntegerStruct;
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
		return IntegerStruct.valueOf(count);
	}
}
