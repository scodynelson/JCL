package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.DeclareAnalyzer;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class BodyWithDeclaresAndDocStringAnalyzer implements Analyzer<BodyProcessingResult, ListStruct> {

	public static final BodyWithDeclaresAndDocStringAnalyzer INSTANCE = new BodyWithDeclaresAndDocStringAnalyzer();

	@Override
	public BodyProcessingResult analyze(final ListStruct input, final SemanticAnalyzer analyzer) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		final List<ListStruct> declarations = new ArrayList<>();
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = bodyJavaList.iterator();
		if (iterator.hasNext()) {

			LispStruct next = iterator.next();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {
				final ListStruct analyzedDeclaration = DeclareAnalyzer.INSTANCE.analyze((ListStruct) next, analyzer);
				declarations.add(analyzedDeclaration);
				next = iterator.next();
			}

			if ((next instanceof StringStruct) && iterator.hasNext()) {
				docString = (StringStruct) next; // No need to analyze this
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				final LispStruct analyzedForm = analyzer.analyzeForm(next);
				bodyForms.add(analyzedForm);
				next = iterator.next();
			}

			// Make sure to analyze and add the last form!!
			final LispStruct analyzedForm = analyzer.analyzeForm(next);
			bodyForms.add(analyzedForm);
		}

		return new BodyProcessingResult(declarations, docString, bodyForms);
	}
}
