package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteListAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final QuoteListAnalyzer INSTANCE = new QuoteListAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {
		final SymbolStruct<?> listFnSym;
		if (input.isDotted()) {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST*").getSymbolStruct();
		} else {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST").getSymbolStruct();
		}

		final List<LispStruct> transformedForms = new ArrayList<>();

		final List<LispStruct> formJavaList = input.getAsJavaList();
		for (final LispStruct currentForm : formJavaList) {
			final LispStruct transformedForm = SemanticAnalyzer.saMainLoop(currentForm);
			transformedForms.add(transformedForm);
		}

		final List<LispStruct> transformedListForms = new ArrayList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedListForms);
	}
}
