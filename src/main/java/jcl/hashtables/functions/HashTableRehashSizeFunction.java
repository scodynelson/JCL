/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigDecimal;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.hashtables.HashTableStruct;
import jcl.numbers.BigFloatStruct;
import org.springframework.stereotype.Component;

@Component
public final class HashTableRehashSizeFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "HASH-TABLE-REHASH-SIZE";
	private static final String HASH_TABLE_ARGUMENT = "HASH-TABLE";

	public HashTableRehashSizeFunction() {
		super("Returns the current rehash size of hash-table.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(HASH_TABLE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final HashTableStruct hashTable = arguments.getRequiredArgument(HASH_TABLE_ARGUMENT, HashTableStruct.class);
		final BigDecimal rehashSize = HashTableStruct.getRehashSize();
		return BigFloatStruct.valueOf(rehashSize);
	}
}
