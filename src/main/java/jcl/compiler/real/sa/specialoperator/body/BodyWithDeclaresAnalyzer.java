package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.DeclareAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@Component
public class BodyWithDeclaresAnalyzer implements Analyzer<BodyProcessingResult, ListStruct> {

	@Autowired
	private DeclareAnalyzer declareAnalyzer;

	@Override
	public BodyProcessingResult analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		final List<ListStruct> declarations = new ArrayList<>();
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = bodyJavaList.iterator();
		if (iterator.hasNext()) {

			LispStruct next = iterator.next();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {
				final ListStruct analyzedDeclaration = declareAnalyzer.analyze(analyzer, (ListStruct) next, analysisBuilder);
				declarations.add(analyzedDeclaration);
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				final LispStruct analyzedForm = analyzer.analyzeForm(next, analysisBuilder);
				bodyForms.add(analyzedForm);
				next = iterator.next();
			}
		}

		return new BodyProcessingResult(declarations, null, bodyForms);
	}
}
