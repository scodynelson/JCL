/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import java.math.BigInteger;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.HashTableStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class HashTableCountFunction extends CommonLispBuiltInFunctionStructBase {

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
		return IntegerStruct.toLispInteger(count);
	}
}
