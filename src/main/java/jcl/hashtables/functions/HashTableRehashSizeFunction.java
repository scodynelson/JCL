/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigDecimal;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.numbers.FloatStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.HashTableType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class HashTableRehashSizeFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public HashTableRehashSizeFunction() {
		super("Returns the current rehash size of hash-table.");
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
		final BigDecimal rehashSize = HashTableStruct.getRehashSize();
		return new FloatStruct(rehashSize);
	}

	@Override
	protected String functionName() {
		return "HASH-TABLE-REHASH-SIZE";
	}
}
