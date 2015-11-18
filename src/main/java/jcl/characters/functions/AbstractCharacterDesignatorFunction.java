/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractCharacterDesignatorFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -748646141505142347L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterDesignatorFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> characterDesignator = GlobalPackageStruct.COMMON_LISP.intern("CHARACTER-DESIGNATOR").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(characterDesignator);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, "CHARACTER", "Character",
				CharacterType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE);

		if (lispStruct instanceof CharacterStruct) {
			return lispStruct;
		} else if (lispStruct instanceof StringStruct) {
			return stringFunction().apply((StringStruct) lispStruct);
		} else if (lispStruct instanceof SymbolStruct) {
			return symbolFunction().apply((SymbolStruct) lispStruct);
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	protected abstract Function<StringStruct, LispStruct> stringFunction();

	protected abstract Function<SymbolStruct<?>, LispStruct> symbolFunction();
}
