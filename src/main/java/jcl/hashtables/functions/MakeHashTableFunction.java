/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.EquatorFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.hashtables.HashTableStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RealStruct;
import jcl.numbers.SingleFloatStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class MakeHashTableFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-HASH-TABLE";
	private static final KeywordStruct TEST = new KeywordStruct("TEST");
	private static final KeywordStruct SIZE = new KeywordStruct("SIZE");
	private static final KeywordStruct REHASH_SIZE = new KeywordStruct("REHASH-SIZE");
	private static final KeywordStruct REHASH_THRESHOLD = new KeywordStruct("REHASH-THRESHOLD");
	private static final FloatStruct DEFAULT_REHASH_THRESHOLD = SingleFloatStruct.valueOf(0.75F);

	public MakeHashTableFunction() {
		super("Creates and returns a new hash table.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .keyParameter(TEST).withInitialValue(CommonLispSymbols.EQL)
		                .keyParameter(SIZE).withInitialValue(IntegerStruct.TEN)
		                .keyParameter(REHASH_SIZE).withInitialValue(IntegerStruct.ONE)
		                .keyParameter(REHASH_THRESHOLD).withInitialValue(DEFAULT_REHASH_THRESHOLD)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct test = arguments.getKeyArgument(TEST);
		final FunctionStruct testFunction = validateFunctionDesignator(test);
		final EquatorFunctionStruct equatorTestFunction = (EquatorFunctionStruct) testFunction;

		final IntegerStruct size = arguments.getKeyArgument(SIZE, IntegerStruct.class);
		final RealStruct rehashSize = arguments.getKeyArgument(REHASH_SIZE, RealStruct.class);
//		new OrTypeSpecifier(
//				IntegerType.Factory.getInstance(BigInteger.ONE, null),
//				FloatType.Factory.getInstance(BigDecimal.ONE, null)
//		)
		// TODO: Float??
		final FloatStruct rehashThreshold = arguments.getKeyArgument(REHASH_THRESHOLD, FloatStruct.class);
//		RealType.Factory.getInstance(BigInteger.ZERO, BigInteger.ONE)

		return new HashTableStruct(equatorTestFunction, size.bigIntegerValue(), rehashThreshold.floatValue());
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
