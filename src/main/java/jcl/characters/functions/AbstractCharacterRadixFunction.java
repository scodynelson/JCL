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

public abstract class AbstractCharacterRadixFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 2367501369456274356L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterRadixFunction(final String documentation) {
		super(documentation);
	}

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
