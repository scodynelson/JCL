/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class NameCharFunction extends FunctionStruct {

	private static final long serialVersionUID = 3406210294951003426L;

	private NameCharFunction() {
		super("Returns the character object whose name is name. If such a character does not exist, nil is returned.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("NAME-CHAR").getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> name = GlobalPackageStruct.COMMON_LISP.intern("NAME").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(name);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct stringDesignator = lispStructs[0];
		if (stringDesignator instanceof CharacterStruct) {
			return stringDesignator;
		} else if (stringDesignator instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) stringDesignator;
			return CharacterStruct.nameChar(stringStruct.getAsJavaString());
		} else if (stringDesignator instanceof SymbolStruct) {
			final SymbolStruct<?> symbolStruct = (SymbolStruct) stringDesignator;
			return CharacterStruct.nameChar(symbolStruct.getName());
		} else {
			throw new TypeErrorException("not character designator");
		}
	}
}
