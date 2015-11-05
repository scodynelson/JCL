/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.sequences.SequenceStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class LengthFunction extends FunctionStruct {

	public static final SymbolStruct<?> LENGTH = GlobalPackageStruct.COMMON_LISP.intern("LENGTH").getSymbol();

	private static final long serialVersionUID = -1598433382342924007L;

	private LengthFunction() {
		super("Returns the number of elements in sequence.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LENGTH.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LENGTH);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("SEQ").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SequenceStruct lispStruct = (SequenceStruct) lispStructs[0];
		return lispStruct.length();
	}
}
