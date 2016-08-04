/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.KeywordStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.EquatorFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeHashTableFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-HASH-TABLE";
	private static final KeywordStructImpl TEST = KeywordStructImpl.valueOf("TEST");
	private static final KeywordStructImpl SIZE =  KeywordStructImpl.valueOf("SIZE");
	private static final KeywordStructImpl REHASH_SIZE = KeywordStructImpl.valueOf("REHASH-SIZE");
	private static final KeywordStructImpl REHASH_THRESHOLD = KeywordStructImpl.valueOf("REHASH-THRESHOLD");
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
