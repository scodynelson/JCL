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
public final class ClrHashFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public ClrHashFunction() {
		super("Removes all entries from hash-table, and then returns that empty hash table.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Hash-Table", HashTableType.INSTANCE);

		final HashTableStruct hashTable = (HashTableStruct) lispStruct;
		hashTable.clrHash();
		return hashTable;
	}

	@Override
	protected String functionName() {
		return "CLRHASH";
	}
}
