/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.HashTableType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class HashTableTestFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -8548103706841647149L;

	@Autowired
	private TypeValidator validator;

	public HashTableTestFunction() {
		super("Returns the test used for comparing keys in hash-table.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Hash-Table", HashTableType.INSTANCE);

		final HashTableStruct hashTable = (HashTableStruct) lispStruct;
		return hashTable.getTest();
	}

	@Override
	protected String functionName() {
		return "HASH-TABLE-TEST";
	}
}
