package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteListAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final QuoteListAnalyzer INSTANCE = new QuoteListAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {
		final SymbolStruct<?> listFnSym;
		if (input.isDotted()) {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST*").getSymbolStruct();
		} else {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST").getSymbolStruct();
		}

		final List<LispStruct> formJavaList = input.getAsJavaList();

		final List<LispStruct> transformedForms = new ArrayList<>(formJavaList.size());
		for (final LispStruct currentForm : formJavaList) {
			final LispStruct transformedForm = analyzer.analyzeForm(currentForm);
			transformedForms.add(transformedForm);
		}

		final List<LispStruct> transformedListForms = new ArrayList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedListForms);
	}
}
