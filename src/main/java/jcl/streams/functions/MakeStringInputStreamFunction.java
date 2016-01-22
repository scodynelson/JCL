/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StringInputStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringInputStreamFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -4772345122518084767L;

	public MakeStringInputStreamFunction() {
		super("Returns an input string stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final StringStruct stringStruct = (StringStruct) lispStructs[0];
		return new StringInputStreamStruct(stringStruct.getAsJavaString());
	}

	@Override
	protected String functionName() {
		return "MAKE-STRING-INPUT-STREAM";
	}
}
