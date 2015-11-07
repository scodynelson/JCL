/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CodeCharFunction extends FunctionStruct {

	private static final long serialVersionUID = -7853448883104247869L;

	@Autowired
	private TypeValidator validator;

	private CodeCharFunction() {
		super("Returns a character with the code attribute given by code. If no such character exists and one cannot be " +
				"created, nil is returned.");
		initLambdaListBindings();
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern("CODE-CHAR").getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final SymbolStruct<?> code = GlobalPackageStruct.COMMON_LISP.intern("CODE").getSymbol();
		final RequiredParameter requiredParameter = new RequiredParameter(code);
		return Collections.singletonList(requiredParameter);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, "CODE-CHAR", "Code", IntegerType.INSTANCE);

		final IntegerStruct code = (IntegerStruct) lispStruct;
		return CharacterStruct.codeChar(code);
	}
}
