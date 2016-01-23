/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.types.HashTableType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class RemHashFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public RemHashFunction() {
		super("Removes the entry for key in hash-table, if any. Returns true if there was such an entry, or false otherwise.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);
		final RequiredParameter keyParameter
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "KEY").build();
		requiredParameters.add(keyParameter);
		final RequiredParameter hashTableParameter
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").build();
		requiredParameters.add(hashTableParameter);
		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct key = lispStructs[0];

		final LispStruct hashTable = lispStructs[1];
		validator.validateTypes(hashTable, functionName(), "Hash-Table", HashTableType.INSTANCE);
		final HashTableStruct hashTableValue = (HashTableStruct) hashTable;

		final LispStruct removedValue = hashTableValue.remHash(key);
		return BooleanStructs.toLispBoolean(removedValue == null);
	}

	@Override
	protected String functionName() {
		return "REMHASH";
	}
}
