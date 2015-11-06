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
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class DigitCharFunction extends FunctionStruct {

	private static final long serialVersionUID = -8526559424608179479L;

	private DigitCharFunction() {
		super("If weight is less than radix, digit-char returns a character which has that weight when considered as a " +
				"digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will " +
				"be an uppercase character. If weight is greater than or equal to radix, digit-char returns false.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("DIGIT-CHAR").getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> weight = GlobalPackageStruct.COMMON_LISP.intern("WEIGHT").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(weight);
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

		final IntegerStruct radix;
		if (lispStructs.length == 2) {
			final LispStruct possibleRadix = lispStructs[1];
			if (possibleRadix instanceof IntegerStruct) {
				radix = (IntegerStruct) possibleRadix;
			} else {
				throw new TypeErrorException("not radix");
			}
		} else {
			radix = IntegerStruct.TEN;
		}

		final LispStruct weight = lispStructs[0];
		if (weight instanceof IntegerStruct) {
			final IntegerStruct struct = (IntegerStruct) weight;
			return CharacterStruct.digitChar(struct, radix);
		} else {
			throw new TypeErrorException("not character designator");
		}
	}
}
