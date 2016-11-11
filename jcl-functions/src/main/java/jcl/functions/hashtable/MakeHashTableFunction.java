/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.functions.EquatorFunctionStructBase;
import jcl.lang.FloatStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.RealStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class MakeHashTableFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MAKE-HASH-TABLE";
	private static final KeywordStruct TEST = LispStructFactory.toKeyword("TEST");
	private static final KeywordStruct SIZE =  LispStructFactory.toKeyword("SIZE");
	private static final KeywordStruct REHASH_SIZE = LispStructFactory.toKeyword("REHASH-SIZE");
	private static final KeywordStruct REHASH_THRESHOLD = LispStructFactory.toKeyword("REHASH-THRESHOLD");
	private static final FloatStruct DEFAULT_REHASH_THRESHOLD = LispStructFactory.toFloat(0.75F);

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
		final EquatorFunctionStructBase equatorTestFunction = (EquatorFunctionStructBase) testFunction;

		final IntegerStruct size = arguments.getKeyArgument(SIZE, IntegerStruct.class);
		final RealStruct rehashSize = arguments.getKeyArgument(REHASH_SIZE, RealStruct.class);
//		new OrTypeSpecifier(
//				IntegerType.Factory.getInstance(BigInteger.ONE, null),
//				FloatType.Factory.getInstance(BigDecimal.ONE, null)
//		)
		// TODO: Float??
		final FloatStruct rehashThreshold = arguments.getKeyArgument(REHASH_THRESHOLD, FloatStruct.class);
//		RealType.Factory.getInstance(BigInteger.ZERO, BigInteger.ONE)

		return LispStructFactory.toHashTable(equatorTestFunction, size.bigIntegerValue(), rehashThreshold.floatValue());
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
