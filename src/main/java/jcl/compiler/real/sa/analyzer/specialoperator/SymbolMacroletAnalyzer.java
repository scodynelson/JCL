package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.SymbolMacroletElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
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
public class SymbolMacroletAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3878455475225336840L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public SymbolMacroletElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final SymbolMacroletEnvironment symbolMacroletEnvironment = new SymbolMacroletEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(symbolMacroletEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct parameters = (ListStruct) second;
			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();
			validateDeclares(declareElement);

			final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

			final List<SymbolMacroletElement.SymbolMacroletElementVar> symbolMacroletVars
					= parametersAsJavaList.stream()
					                      .map(e -> getSymbolMacroletElementVar(e, declareElement, analyzer, analysisBuilder, symbolMacroletEnvironment, environmentStack))
					                      .collect(Collectors.toList());

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<Element> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			return new SymbolMacroletElement(symbolMacroletVars, analyzedBodyForms, symbolMacroletEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static void validateDeclares(final DeclareElement declareElement) {
		if (declareElement != null) {
			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			if (!specialDeclarationElements.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed. Got: " + specialDeclarationElements);
			}
		}
	}

	private static SymbolMacroletElement.SymbolMacroletElementVar getSymbolMacroletElementVar(final LispStruct parameter,
	                                                                                          final DeclareElement declareElement,
	                                                                                          final SemanticAnalyzer analyzer,
	                                                                                          final AnalysisBuilder analysisBuilder,
	                                                                                          final SymbolMacroletEnvironment symbolMacroletEnvironment,
	                                                                                          final EnvironmentStack environmentStack) {

		if (!(parameter instanceof ListStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + parameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final SymbolStruct<?> var = getSymbolMacroletParameterVar(listParameter, environmentStack);
		final Element expansion = getSymbolMacroletParameterExpansion(listParameter, analyzer, analysisBuilder, environmentStack);

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(symbolMacroletEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		// TODO: get rid of scope here
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, Scope.LEXICAL, T.INSTANCE, expansion);
		symbolMacroletEnvironment.addLexicalBinding(binding);

		final SymbolElement<?> varSE = new SymbolElement<>(var);
		return new SymbolMacroletElement.SymbolMacroletElementVar(varSE, expansion);
	}

	private static SymbolStruct<?> getSymbolMacroletParameterVar(final ListStruct listParameter,
	                                                             final EnvironmentStack lexicalEnvironmentStack) {
		final int listParameterSize = listParameter.size();
		if (listParameterSize != 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + listParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
		}

		final SymbolStruct<?> parameterVar = (SymbolStruct) listParameterFirst;

		final Environment globalEnvironment = lexicalEnvironmentStack.firstElement();
		final boolean hasGlobalBinding = globalEnvironment.hasLexicalBinding(parameterVar);
		if (hasGlobalBinding) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element symbol must not exist in the global environment.");
		}

		return parameterVar;
	}

	private static Element getSymbolMacroletParameterExpansion(final ListStruct listParameter,
	                                                           final SemanticAnalyzer analyzer,
	                                                           final AnalysisBuilder analysisBuilder,
	                                                           final EnvironmentStack environmentStack) {

		final LispStruct parameterValue = listParameter.getRest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment currentEnvironment = environmentStack.pop();

		try {
			return analyzer.analyzeForm(parameterValue, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}
	}

}
