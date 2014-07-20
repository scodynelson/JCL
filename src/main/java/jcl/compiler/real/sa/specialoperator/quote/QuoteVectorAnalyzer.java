package jcl.compiler.real.sa.specialoperator.quote;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteVectorAnalyzer implements Analyzer<ListStruct, VectorStruct> {

	public static final QuoteVectorAnalyzer INSTANCE = new QuoteVectorAnalyzer();

	@Override
	public ListStruct analyze(VectorStruct input) {
		final SymbolStruct<?> vectorFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("VECTOR").getSymbolStruct();

		final List<LispStruct> transformedForms = new ArrayList<>();

		final List<LispStruct> formJavaList = input.getContents();
		for (final LispStruct currentForm : formJavaList) {
			final LispStruct transformedForm = SemanticAnalyzer.saMainLoop(currentForm);
			transformedForms.add(transformedForm);
		}

		final List<LispStruct> transformedVectorForms = new ArrayList<>();
		transformedVectorForms.add(vectorFnSym);
		transformedVectorForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedVectorForms);
	}
}
