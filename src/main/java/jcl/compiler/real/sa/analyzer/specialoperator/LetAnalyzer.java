package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.LetElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class LetAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2933802423859476026L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public LetElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
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

			final ListStruct parameters = (ListStruct) second;
			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

			final List<LetElement.LetVar> letVars
					= parametersAsJavaList.stream()
					                      .map(e -> getLetVar(e, declareElement, analyzer, analysisBuilder, letEnvironment, environmentStack))
					                      .collect(Collectors.toList());

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

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

	private static LetElement.LetVar getLetVar(final LispStruct parameter,
	                                           final DeclareElement declareElement,
	                                           final SemanticAnalyzer analyzer,
	                                           final AnalysisBuilder analysisBuilder,
	                                           final LetEnvironment letEnvironment,
	                                           final EnvironmentStack environmentStack) {

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + parameter);
		}

		final SymbolStruct<?> var;
		final Element initForm;

		if (parameter instanceof ListStruct) {
			final ListStruct listParameter = (ListStruct) parameter;
			var = getLetListParameterVar(listParameter);
			initForm = getLetListParameterInitForm(listParameter, analyzer, analysisBuilder, environmentStack);
		} else {
			var = (SymbolStruct) parameter;
			initForm = NullElement.INSTANCE;
		}

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(letEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolElement<?> varSE = new SymbolElement<>(var);
		final boolean isSpecial = isSpecial(declareElement, varSE);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final Scope scope = isSpecial ? Scope.DYNAMIC : Scope.LEXICAL;
		final EnvironmentBinding binding = new EnvironmentBinding(var, allocation, scope, T.INSTANCE, initForm);
		letEnvironment.addLexicalBinding(binding);

		return new LetElement.LetVar(varSE, initForm);
	}

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolElement<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolElement<?> specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}

	private static SymbolStruct<?> getLetListParameterVar(final ListStruct listParameter) {
		final int listParameterSize = listParameter.size();
		if ((listParameterSize < 1) || (listParameterSize > 2)) {
			throw new ProgramErrorException("LET: ListStruct parameter must have only 1 or 2 elements. Got: " + listParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException("LET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
		}
		return (SymbolStruct) listParameterFirst;
	}

	private static Element getLetListParameterInitForm(final ListStruct listParameter,
	                                                   final SemanticAnalyzer analyzer,
	                                                   final AnalysisBuilder analysisBuilder,
	                                                   final EnvironmentStack environmentStack) {

		final LispStruct parameterValue = listParameter.getRest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment currentLexicalEnvironment = environmentStack.pop();

		try {
			return analyzer.analyzeForm(parameterValue, analysisBuilder);
		} finally {
			environmentStack.push(currentLexicalEnvironment);
		}
	}
}
