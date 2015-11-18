/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
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
		final SymbolStruct<?> radix = GlobalPackageStruct.COMMON_LISP.intern("RADIX").getSymbol();

		final SymbolStruct<?> radixSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("RADIX-P").getSymbol();
		final SuppliedPParameter suppliedPParameter = new SuppliedPParameter(radixSuppliedP);

		final OptionalParameter optionalParameter = new OptionalParameter(radix, NullStruct.INSTANCE, suppliedPParameter);
		return Collections.singletonList(optionalParameter);
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
