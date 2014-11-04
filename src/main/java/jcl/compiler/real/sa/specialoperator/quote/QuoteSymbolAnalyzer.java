package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteSymbolAnalyzer implements Analyzer<ListStruct, SymbolStruct<?>> {

	public static final QuoteSymbolAnalyzer INSTANCE = new QuoteSymbolAnalyzer();

	@Override
	public ListStruct analyze(final SymbolStruct<?> input, final SemanticAnalyzer analyzer) {

		final String symbolFunctionString;
		final String symbolNameString;

		final PackageStruct symbolPackage = input.getSymbolPackage();
		if (symbolPackage != null) {
			symbolFunctionString = "FIND-SYMBOL";
			symbolNameString = symbolPackage.getName();
		} else {
			symbolFunctionString = "MAKE-SYMBOL";
			symbolNameString = input.getName();
		}

		final List<LispStruct> symbolQuoteList = new ArrayList<>();
		symbolQuoteList.add(GlobalPackageStruct.COMMON_LISP.findSymbol(symbolFunctionString).getSymbolStruct());
		symbolQuoteList.add(new StringStruct(symbolNameString));

		return ListStruct.buildProperList(symbolQuoteList);
	}
}
