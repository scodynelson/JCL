package jcl.compiler.real.sa.analyzer.specialoperator.body;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.sa.analyzer.specialoperator.declare.DeclareAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@Component
public class BodyWithDeclaresAndDocStringAnalyzer implements Serializable {

	private static final long serialVersionUID = 3031836027613475526L;

	@Autowired
	private DeclareAnalyzer declareAnalyzer;

	public BodyProcessingResult analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		final List<LispStruct> bodyJavaList = input.getAsJavaList();

		DeclareElement declareElement = null;
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = bodyJavaList.iterator();

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			final List<LispStruct> allDeclarations = new ArrayList<>();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {

				final ListStruct declareStatement = (ListStruct) next;
				final ListStruct declarations = declareStatement.getRest();

				allDeclarations.addAll(declarations.getAsJavaList());
				next = iterator.next();
			}

			final ListStruct fullDeclaration = new ConsStruct(SpecialOperator.DECLARE, ListStruct.buildProperList(allDeclarations));
			declareElement = declareAnalyzer.analyze(analyzer, fullDeclaration, analysisBuilder);

			if ((next instanceof StringStruct) && iterator.hasNext()) {
				docString = (StringStruct) next; // No need to analyze this
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				bodyForms.add(next);
				next = iterator.next();
			}

			// Make sure to add the last form!!
			bodyForms.add(next);
		}

		return new BodyProcessingResult(declareElement, docString, bodyForms);
	}
}
