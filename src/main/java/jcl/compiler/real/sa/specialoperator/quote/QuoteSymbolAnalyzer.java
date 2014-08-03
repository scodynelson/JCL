package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.structs.arrays.StringStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteSymbolAnalyzer implements Analyzer<ListStruct, SymbolStruct<?>> {

	public static final QuoteSymbolAnalyzer INSTANCE = new QuoteSymbolAnalyzer();

	@Override
	public ListStruct analyze(final SymbolStruct<?> input) {
		if (input.getSymbolPackage() != null) {
			final List<LispStruct> findSymbolList = new ArrayList<>();
			findSymbolList.add(GlobalPackageStruct.COMMON_LISP.findSymbol("FIND-SYMBOL").getSymbolStruct());
			findSymbolList.add(new StringStruct(input.getSymbolPackage().getName()));

			return ListStruct.buildProperList(findSymbolList);
		} else {
			final List<LispStruct> makeSymbolList = new ArrayList<>();
			makeSymbolList.add(GlobalPackageStruct.COMMON_LISP.findSymbol("MAKE-SYMBOL").getSymbolStruct());
			makeSymbolList.add(new StringStruct(input.getName()));

			return ListStruct.buildProperList(makeSymbolList);
		}
	}
}
