/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SxHashFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -7444850332899583238L;

	@Autowired
	private TypeValidator validator;

	public SxHashFunction() {
		super("Returns a hash code for object");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final int hashCode = lispStruct.hashCode();
		return new IntegerStruct(BigInteger.valueOf(hashCode));
	}

	@Override
	protected String functionName() {
		return "SXHASH";
	}
}
