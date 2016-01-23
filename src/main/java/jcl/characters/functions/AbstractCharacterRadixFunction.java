/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on an optional {@link
 * IntegerStruct} radix value.
 */
abstract class AbstractCharacterRadixFunction extends AbstractCommonLispFunctionStruct {

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
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "RADIX")
		                        .suppliedPBinding()
		                        .buildList();
	}

	/**
	 * Gets the {@link IntegerStruct} radix value from the {@link LispStruct[]} parameters. The radix value is to be
	 * the second value in the parameter list. If there is only a single element in the parameter list, the default
	 * radix value will be {@link IntegerStruct#TEN}.
	 *
	 * @param lispStructs
	 * 		the function parameter list
	 *
	 * @return the radix value from the {@link LispStruct[]} parameters or {@link IntegerStruct#TEN}
	 */
	protected IntegerStruct getRadix(final LispStruct... lispStructs) {
		final IntegerStruct radix;
		if (lispStructs.length >= 2) {
			radix = validator.validateType(lispStructs[1], functionName(), "Radix", IntegerType.INSTANCE, IntegerStruct.class);
		} else {
			radix = IntegerStruct.TEN;
		}
		return radix;
	}
}
