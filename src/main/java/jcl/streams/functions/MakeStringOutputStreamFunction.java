/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.StringOutputStreamStruct;
import jcl.symbols.KeywordStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.CharacterType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringOutputStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public MakeStringOutputStreamFunction() {
		super("Returns an output string stream that accepts characters and makes available (via get-output-stream-string) a string that contains the characters that were actually output.");
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		return KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "ELEMENT-TYPE")
		                   .suppliedPBinding()
		                   .initForm(CharacterType.INSTANCE)
		                   .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 0, CommonLispSymbols.ELEMENT_TYPE_KEYWORD);

		final LispStruct elementType
				= keywords.getOrDefault(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, CharacterType.INSTANCE);
		validator.validateTypes(elementType, functionName(), "Element Type", SymbolType.INSTANCE);

		if (!(elementType instanceof LispType)) {
			final String printedObject = printer.print(elementType);
			throw new TypeErrorException(functionName() + ": Element Type must be an actual TYPE. Got: " + printedObject);
		}

		return new StringOutputStreamStruct((LispType) elementType);
	}

	@Override
	protected String functionName() {
		return "MAKE-STRING-OUTPUT-STREAM";
	}
}
