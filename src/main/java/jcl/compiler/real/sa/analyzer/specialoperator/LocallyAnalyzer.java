package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.specialoperator.LocallyElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.lists.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
public class LocallyAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 8925649944409732052L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public LocallyElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment parentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final LexicalEnvironment locallyEnvironment = new LexicalEnvironment(parentLexicalEnvironment, Marker.LOCALLY, newClosureDepth);
		lexicalEnvironmentStack.push(locallyEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct bodyForms = input.getRest();
			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);

			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();
//			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
//			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, analysisBuilder, locallyEnvironment));

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<Element> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

			return new LocallyElement(analyzedBodyForms, currentLexicalEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			lexicalEnvironmentStack.pop();
		}
	}

//	private static void addDynamicVariableBinding(final SpecialDeclarationElement specialDeclarationElement,
//	                                              final AnalysisBuilder analysisBuilder,
//	                                              final LocallyEnvironment locallyEnvironment) {
//
//		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(locallyEnvironment);
//		analysisBuilder.setBindingsPosition(newBindingsPosition);
//
//		final SymbolStruct<?> var = specialDeclarationElement.getVar().getSymbolStruct();
//
//		final Environment bindingEnvironment = getBindingEnvironment(var, locallyEnvironment);
//		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);
//
//		final DynamicBinding binding = new DynamicBinding(allocation, var, T.INSTANCE);
//		locallyEnvironment.addDynamicBinding(binding);
//	}
//
//	private static Environment getBindingEnvironment(final SymbolStruct<?> var,
//	                                                 final Environment environment) {
//
//		Environment currentEnvironment = environment;
//
//		while (!currentEnvironment.equals(Environment.NULL)) {
//
//			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);
//			if (hasDynamicBinding) {
//				break;
//			}
//
//			currentEnvironment = currentEnvironment.getParent();
//		}
//
//		return currentEnvironment;
//	}
}
