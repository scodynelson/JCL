/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.types.HashTableType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MapHashFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -5287826548927565885L;

	@Autowired
	private TypeValidator validator;

	public MapHashFunction() {
		super("Iterates over all entries in the hash-table. For each entry, the function is called with two arguments--the key and the value of that entry.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);
		final RequiredParameter functionParameter
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "FUNCTION").build();
		requiredParameters.add(functionParameter);
		final RequiredParameter hashTableParameter
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "HASH-TABLE").build();
		requiredParameters.add(hashTableParameter);
		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct function = lispStructs[0];
		final FunctionStruct functionParam = validator.validateFunctionDesignator(function, functionName(), "Function");

		final LispStruct hashTable = lispStructs[1];
		validator.validateTypes(hashTable, functionName(), "Hash-Table", HashTableType.INSTANCE);

		final HashTableStruct hashTableValue = (HashTableStruct) hashTable;
		hashTableValue.mapHash(functionParam);
		return NILStruct.INSTANCE;
	}

	@Override
	protected String functionName() {
		return "MAPHASH";
	}
}