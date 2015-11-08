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
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class DigitCharPFunction extends AbstractCharacterRadixFunction {

	private static final long serialVersionUID = -7883462040539135670L;

	@Autowired
	private TypeValidator validator;

	private DigitCharPFunction() {
		super("Tests whether character is a digit in the specified radix. If it is a digit in that radix, its weight is " +
				"returned as an integer; otherwise nil is returned.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> character = GlobalPackageStruct.COMMON_LISP.intern("CHARACTER").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(character);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);
		final CharacterStruct character = (CharacterStruct) lispStruct;

		final IntegerStruct radix = getRadix(lispStructs);
		return character.charDigit(radix);
	}

	@Override
	protected String functionName() {
		return "DIGIT-CHAR-P";
	}
}
