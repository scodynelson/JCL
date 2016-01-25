/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.BooleanType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CloseFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public CloseFunction() {
		super("Closes Stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		return KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "ABORT")
		                   .suppliedPBinding()
		                   .initForm(NILStruct.INSTANCE)
		                   .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final Map<KeywordStruct, LispStruct> keywords = getKeywords(lispStructs, 1, CommonLispSymbols.ABORT_KEYWORD);

		final LispStruct abort
				= keywords.getOrDefault(CommonLispSymbols.ABORT_KEYWORD, NILStruct.INSTANCE);
		validator.validateTypes(abort, functionName(), "Abort", BooleanType.INSTANCE);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Stream", StreamType.INSTANCE);

		final StreamStruct streamStruct = (StreamStruct) lispStruct;
		final BooleanStruct abortValue = (BooleanStruct) abort;

		final boolean wasClosed = streamStruct.close(abortValue.booleanValue());
		return BooleanStructs.toLispBoolean(wasClosed);
	}

	@Override
	protected String functionName() {
		return "CLOSE";
	}
}
