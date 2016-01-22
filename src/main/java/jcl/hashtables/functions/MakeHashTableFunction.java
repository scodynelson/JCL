/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.EquatorFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.FloatType;
import jcl.types.IntegerType;
import jcl.types.RealType;
import jcl.types.TypeValidator;
import jcl.types.typespecifiers.OrTypeSpecifier;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeHashTableFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -6761797908599450463L;

	@Autowired
	private TypeValidator validator;

	private static final KeywordStruct TEST = new KeywordStruct("TEST");

	private static final KeywordStruct SIZE = new KeywordStruct("SIZE");

	private static final KeywordStruct REHASH_SIZE = new KeywordStruct("REHASH-SIZE");

	private static final KeywordStruct REHASH_THRESHOLD = new KeywordStruct("REHASH-THRESHOLD");

	private static final FloatStruct DEFAULT_REHASH_THRESHOLD = new FloatStruct(BigDecimal.valueOf(0.75F));

	public MakeHashTableFunction() {
		super("Creates and returns a new hash table.");
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyParameters = new ArrayList<>(4);

		final KeyParameter testParameter
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "TEST")
				              .initForm(CommonLispSymbols.EQL)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(testParameter);

		final KeyParameter sizeParameter
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "SIZE")
				              .initForm(IntegerStruct.TEN)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(sizeParameter);

		final KeyParameter rehashSizeParameter
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "REHASH-SIZE")
				              .initForm(IntegerStruct.ONE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(rehashSizeParameter);

		final KeyParameter rehashThresholdParameter
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "REHASH-THRESHOLD")
				              .initForm(DEFAULT_REHASH_THRESHOLD)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(rehashThresholdParameter);

		return keyParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 0, TEST, SIZE, REHASH_SIZE, REHASH_THRESHOLD);

		final LispStruct test
				= keywords.getOrDefault(TEST, CommonLispSymbols.EQL);
		final FunctionStruct testFunction = validator.validateFunctionDesignator(test, functionName(), "Test");
		if (!(testFunction instanceof EquatorFunctionStruct)) {
			throw new TypeErrorException("Function must be an equator function: EQ, EQL, EQUAL, or EQUALP.");
		}
		final EquatorFunctionStruct equatorTestFunction = (EquatorFunctionStruct) testFunction;

		final LispStruct size
				= keywords.getOrDefault(SIZE, IntegerStruct.TEN);
		validator.validateTypes(size, functionName(), "Size", IntegerType.INSTANCE);
		final IntegerStruct sizeValue = (IntegerStruct) size;

		final LispStruct rehashSize
				= keywords.getOrDefault(REHASH_SIZE, IntegerStruct.ONE);
		validator.validateTypes(rehashSize, functionName(), "Rehash Size",
				new OrTypeSpecifier(
						IntegerType.Factory.getInstance(BigInteger.ONE, null),
						FloatType.Factory.getInstance(BigDecimal.ONE, null)
				)
		);

		final LispStruct rehashThreshold
				= keywords.getOrDefault(REHASH_THRESHOLD, DEFAULT_REHASH_THRESHOLD);
		validator.validateTypes(rehashThreshold, functionName(), "Rehash Threshold",
				RealType.Factory.getInstance(BigInteger.ZERO, BigInteger.ONE));
		final RealStruct rehashThresholdValue = (RealStruct) rehashThreshold;

		return new HashTableStruct(equatorTestFunction, sizeValue.getBigInteger(), rehashThresholdValue.bigDecimalValue());
	}

	@Override
	protected String functionName() {
		return "MAKE-HASH-TABLE";
	}
}
