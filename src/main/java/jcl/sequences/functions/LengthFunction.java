/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences.functions;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.sequences.SequenceStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LengthFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public LengthFunction() {
		super("Returns the number of elements in sequence.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SEQUENCE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SequenceStruct sequence
				= validator.validateType(lispStructs[0], functionName(), "Sequence", ListType.INSTANCE, SequenceStruct.class);
		final Long length = sequence.length();
		return new IntegerStruct(BigInteger.valueOf(length));
	}

	@Override
	protected String functionName() {
		return "LENGTH";
	}
}
