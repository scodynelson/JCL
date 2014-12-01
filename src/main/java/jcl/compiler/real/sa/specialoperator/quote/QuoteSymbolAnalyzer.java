package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.arrays.StringStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class QuoteSymbolAnalyzer implements Analyzer<ListStruct, SymbolStruct<?>> {

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
