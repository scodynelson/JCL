package jcl.compiler.real.sa.analyzer.body;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.MacroExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BodyWithDeclaresAndDocStringAnalyzer implements Serializable {

	private static final long serialVersionUID = 3031836027613475526L;

	@Autowired
	private MacroExpander<DeclareStruct, ListStruct> declareExpander;

	public BodyProcessingResult analyze(final List<LispStruct> input, final AnalysisBuilder analysisBuilder) {

		DeclareStruct declareElement = null;
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = input.iterator();

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			final List<LispStruct> allDeclarations = new ArrayList<>();
			allDeclarations.add(SpecialOperator.DECLARE);

			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {

				final ListStruct declareStatement = (ListStruct) next;
				final List<LispStruct> declarations = declareStatement.getRest().getAsJavaList();

				allDeclarations.addAll(declarations);
				next = iterator.next();
			}

			final ListStruct fullDeclaration = ListStruct.buildProperList(allDeclarations);
			declareElement = declareExpander.expand(fullDeclaration, analysisBuilder);

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