/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractCharacterFunction extends FunctionStruct {

	private static final long serialVersionUID = -7030656974789702740L;

	@Autowired
	private TypeValidator validator;

	protected AbstractCharacterFunction(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern(functionName()).getSymbol();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final CharacterStruct character = getCharacter(lispStructs);
		return characterFunction().apply(character);
	}

	private CharacterStruct getCharacter(final LispStruct... lispStructs) {

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);

		return (CharacterStruct) lispStruct;
	}

	protected abstract String functionName();

	protected abstract Function<CharacterStruct, LispStruct> characterFunction();
}
