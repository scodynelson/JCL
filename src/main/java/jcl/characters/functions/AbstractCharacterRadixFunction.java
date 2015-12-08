/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

abstract class AbstractCharacterRadixFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2367501369456274356L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractCharacterRadixFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link OptionalParameter} integer object for this function.
	 *
	 * @return a list of a single {@link OptionalParameter} integer object
	 */
	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "RADIX")
				.suppliedPBinding()
				.buildList();
	}

	protected IntegerStruct getRadix(final LispStruct[] lispStructs) {
		final IntegerStruct radix;
		if (lispStructs.length == 2) {
			final LispStruct possibleRadix = lispStructs[1];
			validator.validateTypes(possibleRadix, functionName(), "Radix", IntegerType.INSTANCE);

			radix = (IntegerStruct) possibleRadix;
		} else {
			radix = IntegerStruct.TEN;
		}
		return radix;
	}
}
