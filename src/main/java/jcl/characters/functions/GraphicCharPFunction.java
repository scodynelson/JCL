/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class GraphicCharPFunction extends FunctionStruct {

	private static final long serialVersionUID = 8709892843954599169L;

	private GraphicCharPFunction() {
		super("Returns true if character is a graphic character; otherwise, returns false.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("GRAPHIC-CHAR-P").getSymbol();
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

		final LispStruct character = lispStructs[0];
		if (character instanceof CharacterStruct) {
			final CharacterStruct struct = (CharacterStruct) character;
			final boolean graphicChar = struct.isGraphicChar();
			return BooleanStructs.toLispBoolean(graphicChar);
		} else {
			throw new TypeErrorException("not character designator");
		}
	}
}
