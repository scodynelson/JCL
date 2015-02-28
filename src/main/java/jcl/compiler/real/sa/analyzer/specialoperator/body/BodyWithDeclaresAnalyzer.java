package jcl.compiler.real.sa.analyzer.specialoperator.body;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.specialoperator.declare.DeclareAnalyzer;
import jcl.system.EnhancedLinkedList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@Component
public class BodyWithDeclaresAnalyzer implements Serializable {

	private static final long serialVersionUID = -4533785417061599823L;

	@Autowired
	private DeclareAnalyzer declareAnalyzer;

	public BodyProcessingResult analyze(final EnhancedLinkedList<SimpleElement> input, final AnalysisBuilder analysisBuilder) {

		DeclareElement declareElement = null;
		final List<SimpleElement> bodyForms = new ArrayList<>();

		final Iterator<SimpleElement> iterator = input.iterator();

		if (iterator.hasNext()) {
			SimpleElement next = iterator.next();

			final EnhancedLinkedList<SimpleElement> allDeclarations = new EnhancedLinkedList<>();
			allDeclarations.add(SpecialOperatorElement.DECLARE);

			while (iterator.hasNext() && (next instanceof ConsElement) && ((ConsElement) next).getElements().getFirst().equals(SpecialOperatorElement.DECLARE)) {

				final ConsElement declareStatement = (ConsElement) next;
				final EnhancedLinkedList<SimpleElement> declarations = declareStatement.getElements().getAllButFirst();

				allDeclarations.addAll(declarations);
				next = iterator.next();
			}

			final ConsElement fullDeclaration = new ConsElement(allDeclarations);
			declareElement = declareAnalyzer.analyze(fullDeclaration, analysisBuilder);

			while (iterator.hasNext()) {
				bodyForms.add(next);
				next = iterator.next();
			}

			// Make sure to add the last form!!
			bodyForms.add(next);
		}

		return new BodyProcessingResult(declareElement, null, bodyForms);
	}
}
