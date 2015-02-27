package jcl.compiler.real.sa.analyzer.specialoperator.lambda;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class LambdaAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Override
	public LambdaElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("LAMBDA: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(lambdaEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListElement parameters = (ListElement) second;
			final EnhancedLinkedList<SimpleElement> bodyForms = inputRest.getAllButFirst();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(analyzer, analysisBuilder, parameters, declareElement);
			final EnhancedLinkedList<SimpleElement> newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList);

			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, analysisBuilder, lambdaEnvironment));

			final List<SimpleElement> realBodyForms = bodyProcessingResult.getBodyForms();
			newStartingLambdaBody.addAll(realBodyForms);

			final ConsElement newLambdaBodyListStruct = new ConsElement(newStartingLambdaBody);

			final Element analyzedBodyForms = analyzer.analyzeForm(newLambdaBodyListStruct, analysisBuilder);
			return new LambdaElement(parsedLambdaList, bodyProcessingResult.getDocString(), analyzedBodyForms, lambdaEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static EnhancedLinkedList<SimpleElement> getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList) {
		final EnhancedLinkedList<SimpleElement> newLambdaBody = new EnhancedLinkedList<>();

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			newLambdaBody.add(SpecialOperatorElement.PROGN);
		} else {
			newLambdaBody.add(SpecialOperatorElement.LET_STAR);

			final List<ConsElement> auxLetStarVars = auxBindings
					.stream()
					.map(e -> new ConsElement(e.getSymbolStruct(), e.getInitForm()))
					.collect(Collectors.toList());

			final ConsElement auxLetStarVarsLL = new ConsElement(new EnhancedLinkedList<>(auxLetStarVars));
			newLambdaBody.add(auxLetStarVarsLL);
		}

		final EnhancedLinkedList<SimpleElement> initFormIfSetqs = getInitFormIfSetqs(parsedLambdaList);
		newLambdaBody.addAll(initFormIfSetqs);

		return newLambdaBody;
	}

	private static EnhancedLinkedList<SimpleElement> getInitFormIfSetqs(final OrdinaryLambdaListBindings parsedLambdaList) {

		final EnhancedLinkedList<SimpleElement> initFormIfSetqs = new EnhancedLinkedList<>();

		final List<OptionalBinding> optionalBindings = parsedLambdaList.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();

			final EnhancedLinkedList<SimpleElement> initFormIfSetq = new EnhancedLinkedList<>();
			initFormIfSetq.add(SpecialOperatorElement.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final EnhancedLinkedList<SimpleElement> initFormSetq = new EnhancedLinkedList<>();
			initFormSetq.add(SpecialOperatorElement.SETQ);
			initFormSetq.add(optionalBinding.getSymbolStruct());
			initFormSetq.add(optionalBinding.getInitForm());

			final ConsElement initFormSetqLL = new ConsElement(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ConsElement initFormIfSetqLL = new ConsElement(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		final List<KeyBinding> keyBindings = parsedLambdaList.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();

			final EnhancedLinkedList<SimpleElement> initFormIfSetq = new EnhancedLinkedList<>();
			initFormIfSetq.add(SpecialOperatorElement.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final EnhancedLinkedList<SimpleElement> initFormSetq = new EnhancedLinkedList<>();
			initFormSetq.add(SpecialOperatorElement.SETQ);
			initFormSetq.add(keyBinding.getSymbolStruct());
			initFormSetq.add(keyBinding.getInitForm());

			final ConsElement initFormSetqLL = new ConsElement(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ConsElement initFormIfSetqLL = new ConsElement(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		return initFormIfSetqs;
	}

	private static void addDynamicVariableBinding(final SpecialDeclarationElement specialDeclarationElement,
	                                              final AnalysisBuilder analysisBuilder,
	                                              final LambdaEnvironment lambdaEnvironment) {

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(lambdaEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolElement var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(lambdaEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		lambdaEnvironment.addDynamicBinding(binding);
	}
}
