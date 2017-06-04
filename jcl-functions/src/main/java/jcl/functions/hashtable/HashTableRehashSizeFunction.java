/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import java.math.BigDecimal;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.DoubleFloatStruct;
import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class HashTableRehashSizeFunction extends CommonLispBuiltInFunctionStructBase {

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
		final BigDecimal rehashSize = hashTable.getRehashSize();
		return DoubleFloatStruct.toLispFloat(rehashSize.doubleValue());
	}
}
