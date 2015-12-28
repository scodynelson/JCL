/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.types.HashTableType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GetHashFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -2780138086216751495L;

	@Autowired
	private TypeValidator validator;

	public GetHashFunction() {
		super("Retrieve the value in the hash-table whose key is the same as the key under the hash-table's equivalence test. If there is no such entry, the result is the default.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);
		final RequiredParameter keyParameter
				= new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "KEY").build();
		requiredParameters.add(keyParameter);
		final RequiredParameter hashTableParameter
				= new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").build();
		requiredParameters.add(hashTableParameter);
		return requiredParameters;
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "DEFAULT")
				.suppliedPBinding()
				.buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct key = lispStructs[0];

		final LispStruct hashTable = lispStructs[1];
		validator.validateTypes(hashTable, functionName(), "Hash-Table", HashTableType.INSTANCE);
		final HashTableStruct hashTableValue = (HashTableStruct) hashTable;

		LispStruct defaultValue = NILStruct.INSTANCE;
		if (lispStructs.length > 2) {
			defaultValue = lispStructs[2];
		}

		final LispStruct hash = hashTableValue.getHash(key);
		return (hash == null) ? defaultValue : hash;
	}

	@Override
	protected String functionName() {
		return "GETHASH";
	}
}
