/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class DigitCharFunction extends AbstractCharacterRadixFunction {

	private static final long serialVersionUID = -8526559424608179479L;

	@Autowired
	private TypeValidator validator;

	private DigitCharFunction() {
		super("If weight is less than radix, digit-char returns a character which has that weight when considered as a " +
				"digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will " +
				"be an uppercase character. If weight is greater than or equal to radix, digit-char returns false.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> weight = GlobalPackageStruct.COMMON_LISP.intern("WEIGHT").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(weight);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Weight", IntegerType.INSTANCE);
		final IntegerStruct weight = (IntegerStruct) lispStruct;

		final IntegerStruct radix = getRadix(lispStructs);
		return CharacterStruct.digitChar(weight, radix);
	}

	@Override
	protected String functionName() {
		return "DIGIT-CHAR";
	}
}
