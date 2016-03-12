/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.CharacterType;
import jcl.types.StreamType;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FileStringLengthFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public FileStringLengthFunction() {
		super("Returns the difference between what (file-position stream) would be after writing object and its current value, or nil if this cannot be determined.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);

		final RequiredParameter stream =
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").build();
		requiredParameters.add(stream);

		final RequiredParameter object =
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").build();
		requiredParameters.add(object);

		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		validator.validateTypes(lispStructs[0], functionName(), "Stream", StreamType.INSTANCE);

		final LispStruct lispStruct2 = lispStructs[1];
		validator.validateTypes(lispStruct2, functionName(), "Object", CharacterType.INSTANCE, StringType.INSTANCE);

		if (lispStruct2 instanceof CharacterStruct) {
			return IntegerStruct.ONE;
		} else if (lispStruct2 instanceof StringStruct) {
			final Long length = ((StringStruct) lispStruct2).length();
			return new IntegerStruct(BigInteger.valueOf(length));
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	@Override
	protected String functionName() {
		return "FILE-STRING-LENGTH";
	}
}
