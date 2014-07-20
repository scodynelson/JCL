package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteSymbolAnalyzer implements Analyzer<ListStruct, SymbolStruct> {

	public static final QuoteSymbolAnalyzer INSTANCE = new QuoteSymbolAnalyzer();

	@Override
	public ListStruct analyze(SymbolStruct input) {
		if (input.getSymbolPackage() != null) {
			// gen (intern "sym name" (find-package "pkg name"))
			final List<LispStruct> symbolFindSymbolPattern = new ArrayList<>();
			symbolFindSymbolPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("FIND-PACKAGE").getSymbolStruct());
			symbolFindSymbolPattern.add(new StringStruct(input.getSymbolPackage().getName()));
			final ListStruct symbolFindSymbolPatternList = ListStruct.buildProperList(symbolFindSymbolPattern);

			final List<LispStruct> symbolInternPattern = new ArrayList<>();
			symbolInternPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("INTERN").getSymbolStruct());
			symbolInternPattern.add(new StringStruct(input.getName()));
			symbolInternPattern.add(symbolFindSymbolPatternList);

			return ListStruct.buildProperList(symbolInternPattern);
		} else {
			// gen (make-symbol "sym name")
			final List<LispStruct> symbolMakeSymbolPattern = new ArrayList<>();
			symbolMakeSymbolPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("MAKE-SYMBOL").getSymbolStruct());
			symbolMakeSymbolPattern.add(new StringStruct(input.getName()));
			final ListStruct symbolMakeSymbolPatternList = ListStruct.buildProperList(symbolMakeSymbolPattern);
			return ListStruct.buildProperList(symbolMakeSymbolPatternList);
		}
	}
}
