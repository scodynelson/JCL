/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import jcl.LispStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.collections4.iterators.ArrayIterator;

public abstract class AbstractCommonLispFunctionStruct extends FunctionStruct {

	private static final long serialVersionUID = 4937708575332457902L;

	protected AbstractCommonLispFunctionStruct(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);
		return null;
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct symbol = aPackage.intern(functionName()).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	protected static Map<KeywordStruct, LispStruct> getKeywords(final LispStruct[] lispStructs, final int keysStart,
	                                                            final KeywordStruct... keywords) {

		final Map<KeywordStruct, LispStruct> keywordMap = new HashMap<>();
		final Iterator<LispStruct> iterator = new ArrayIterator<>(lispStructs, keysStart);

		while (iterator.hasNext()) {
			final LispStruct possibleKeyword = iterator.next();
			if (!(possibleKeyword instanceof KeywordStruct)) {
				break;
			}

			for (final KeywordStruct keyword : keywords) {
				if (keyword.equals(possibleKeyword)) {
					keywordMap.put(keyword, iterator.next());
				}
			}
		}

		return keywordMap;
	}

	protected abstract String functionName();
}
