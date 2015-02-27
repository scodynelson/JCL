package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.KeywordSymbolStruct;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class EvalWhenAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -7301369273443154417L;

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(KeywordOld.CompileToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.LoadToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.Execute);
	}

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {
		return analyze(analyzer, input, analysisBuilder, false, false);
	}

	public Element analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder, final boolean isTopLevel,
	                       final boolean isCompileOrCompileFile) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("EVAL-WHEN: Situation list must be of type ListStruct. Got: " + second);
		}

		final ListElement situationList = (ListElement) second;
		final List<? extends SimpleElement> situationJavaList = situationList.getElements();

		// TODO: fix Keyword check here
		final Collection<? extends SimpleElement> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOP-LEVEL', ':LOAD-TIME-LEVEL', or ':EXECUTE'. Got: " + situationList);
		}

		final EnhancedLinkedList<SimpleElement> forms = inputRest.getAllButFirst();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				// (eval `(progn ,@body)))
//				final ListStruct prognBody = new ConsStruct(SpecialOperator.PROGN, forms);

				// TODO: what we need to do here is:
				// TODO: 1.) Get global instance of 'EVAL' function
				// TODO: 2.) Pass the new 'prognBody' to the 'EVAL' function
				// TODO: 3.) Forcefully evaluate the 'EVAL' function

			}

			if (isLoadTopLevel(situationJavaList) || (!isCompileOrCompileFile && isExecute(situationJavaList))) {
				// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
				final List<Element> analyzedForms =
						forms.stream()
						             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
						             .collect(Collectors.toList());

				// TODO: what we need to do here is:
				// TODO: 1.) Create a new 'LAMBDA' function
				// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
				// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

			}
		} else if (isExecute(situationJavaList)) {
			// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
			final List<Element> analyzedForms =
					forms.stream()
					             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					             .collect(Collectors.toList());

			// TODO: what we need to do here is:
			// TODO: 1.) Create a new 'LAMBDA' function
			// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
			// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

		}

		// TODO: Really, we just do nothing. Should we actually do a 'void' return here???
		return NullElement.INSTANCE;
	}

	private static boolean isCompileTopLevel(final List<? extends SimpleElement> situationList) {
		return situationList.contains(KeywordOld.CompileToplevel);
	}

	private static boolean isLoadTopLevel(final List<? extends SimpleElement> situationList) {
		return situationList.contains(KeywordOld.LoadToplevel);
	}

	private static boolean isExecute(final List<? extends SimpleElement> situationList) {
		return situationList.contains(KeywordOld.Execute);
	}
}
