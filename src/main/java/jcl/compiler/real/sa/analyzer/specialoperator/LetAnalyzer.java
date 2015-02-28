package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LetElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class LetAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2933802423859476026L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LET.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public LetElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("LET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final LetEnvironment letEnvironment = new LetEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(letEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListElement parameters = (ListElement) second;
			final EnhancedLinkedList<SimpleElement> bodyForms = inputRest.getAllButFirst();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze( bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final List<? extends SimpleElement> parametersAsJavaList = parameters.getElements();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final List<LetElement.LetVar> letVars
					= parametersAsJavaList.stream()
					                      .map(e -> getLetVar(e, declareElement, analyzer, analysisBuilder, letEnvironment, environmentStack))
					                      .collect(Collectors.toList());

			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, analysisBuilder, letEnvironment));

			final List<SimpleElement> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<Element> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			return new LetElement(letVars, analyzedBodyForms, letEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static LetElement.LetVar getLetVar(final SimpleElement parameter,
	                                           final DeclareElement declareElement,
	                                           final SemanticAnalyzer analyzer,
	                                           final AnalysisBuilder analysisBuilder,
	                                           final LetEnvironment letEnvironment,
	                                           final EnvironmentStack environmentStack) {

		if (!(parameter instanceof SymbolElement) && !(parameter instanceof ConsElement)) {
			throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + parameter);
		}

		final SymbolElement var;
		final Element initForm;

		if (parameter instanceof ConsElement) {
			final ConsElement listParameter = (ConsElement) parameter;
			var = getLetListParameterVar(listParameter);
			initForm = getLetListParameterInitForm(listParameter, analyzer, analysisBuilder, environmentStack);
		} else {
			var = (SymbolElement) parameter;
			initForm = NullElement.INSTANCE;
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(letEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = isSpecial(declareElement, var);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, initForm);
		if (isSpecial) {
			letEnvironment.addDynamicBinding(binding);
		} else {
			letEnvironment.addLexicalBinding(binding);
		}

		return new LetElement.LetVar(var, initForm);
	}

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolElement var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolElement specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}

	private static SymbolElement getLetListParameterVar(final ConsElement listParameter) {
		final int listParameterSize = listParameter.getElements().size();
		if ((listParameterSize < 1) || (listParameterSize > 2)) {
			throw new ProgramErrorException("LET: ListStruct parameter must have only 1 or 2 elements. Got: " + listParameter);
		}

		final SimpleElement listParameterFirst = listParameter.getElements().getFirst();
		if (!(listParameterFirst instanceof SymbolElement)) {
			throw new ProgramErrorException("LET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
		}
		return (SymbolElement) listParameterFirst;
	}

	private static Element getLetListParameterInitForm(final ConsElement listParameter,
	                                                   final SemanticAnalyzer analyzer,
	                                                   final AnalysisBuilder analysisBuilder,
	                                                   final EnvironmentStack environmentStack) {

		final SimpleElement parameterValue = listParameter.getElements().getAllButFirst().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment currentEnvironment = environmentStack.pop();

		try {
			return analyzer.analyzeForm(parameterValue, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}
	}

	private static void addDynamicVariableBinding(final SpecialDeclarationElement specialDeclarationElement,
	                                              final AnalysisBuilder analysisBuilder,
	                                              final LetEnvironment letEnvironment) {

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(letEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolElement var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(letEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		letEnvironment.addDynamicBinding(binding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
