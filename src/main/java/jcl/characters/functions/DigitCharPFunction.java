/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class DigitCharPFunction extends FunctionStruct {

	private static final long serialVersionUID = -7883462040539135670L;

	@Autowired
	private TypeValidator validator;

	private DigitCharPFunction() {
		super("Tests whether character is a digit in the specified radix. If it is a digit in that radix, its weight is " +
				"returned as an integer; otherwise nil is returned.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("DIGIT-CHAR-P").getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> character = GlobalPackageStruct.COMMON_LISP.intern("CHARACTER").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(character);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final SymbolStruct<?> radix = GlobalPackageStruct.COMMON_LISP.intern("RADIX").getSymbol();

		final SymbolStruct<?> radixSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("RADIX-P").getSymbol();
		final SuppliedPParameter suppliedPParameter = new SuppliedPParameter(radixSuppliedP);

		final OptionalParameter optionalParameter = new OptionalParameter(radix, NullStruct.INSTANCE, suppliedPParameter);
		return Collections.singletonList(optionalParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, "DIGIT-CHAR-P", "Character", CharacterType.INSTANCE);
		final CharacterStruct character = (CharacterStruct) lispStruct;

		final IntegerStruct radix;
		if (lispStructs.length == 2) {
			final LispStruct possibleRadix = lispStructs[1];
			validator.validateTypes(possibleRadix, "DIGIT-CHAR-P", "Radix", IntegerType.INSTANCE);

			radix = (IntegerStruct) possibleRadix;
		} else {
			radix = IntegerStruct.TEN;
		}

		return character.charDigit(radix);
	}
}
