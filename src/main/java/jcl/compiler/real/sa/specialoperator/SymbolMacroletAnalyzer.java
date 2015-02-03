package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.SymbolMacroletElement;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.compiler.real.sa.element.declaration.SpecialDeclarationElement;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
public class SymbolMacroletAnalyzer implements SpecialOperatorAnalyzer {

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

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment parentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final LexicalEnvironment symbolMacroletEnvironment = new LexicalEnvironment(parentLexicalEnvironment, Marker.SYMBOL_MACROLET, newClosureDepth);
		lexicalEnvironmentStack.push(symbolMacroletEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct parameters = (ListStruct) second;
			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();
			validateDeclares(declareElement);


			final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

			final List<SymbolMacroletElement.SymbolMacroletElementVar> letVars
					= parametersAsJavaList.stream()
					                      .map(e -> getSymbolMacroletElementVar(e, declareElement, analyzer, analysisBuilder, lexicalEnvironmentStack))
					                      .collect(Collectors.toList());


			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<LispStruct> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

			return new SymbolMacroletElement(letVars, analyzedBodyForms, currentLexicalEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			lexicalEnvironmentStack.pop();
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
	                                                                                          final Stack<LexicalEnvironment> lexicalEnvironmentStack) {

		if (!(parameter instanceof ListStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + parameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final SymbolStruct<?> var = getSymbolMacroletParameterVar(listParameter, lexicalEnvironmentStack);
		final LispStruct expansion = getSymbolMacroletParameterExpansion(listParameter, analyzer, analysisBuilder, lexicalEnvironmentStack);

		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentLexicalEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		currentLexicalEnvironment.addBinding(var, allocation, expansion, false);

		return new SymbolMacroletElement.SymbolMacroletElementVar(var, expansion);
	}

	private static SymbolStruct<?> getSymbolMacroletParameterVar(final ListStruct listParameter,
	                                                             final Stack<LexicalEnvironment> lexicalEnvironmentStack) {
		final int listParameterSize = listParameter.size();
		if (listParameterSize != 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + listParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
		}

		final SymbolStruct<?> parameterVar = (SymbolStruct) listParameterFirst;

		final LexicalEnvironment globalEnvironment = lexicalEnvironmentStack.firstElement();
		final boolean hasGlobalBinding = globalEnvironment.hasBinding(parameterVar);
		if (hasGlobalBinding) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element symbol must not exist in the global environment.");
		}

		return parameterVar;
	}

	private static LispStruct getSymbolMacroletParameterExpansion(final ListStruct listParameter,
	                                                              final SemanticAnalyzer analyzer,
	                                                              final AnalysisBuilder analysisBuilder,
	                                                              final Stack<LexicalEnvironment> lexicalEnvironmentStack) {

		final LispStruct parameterValue = listParameter.getRest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.pop();

		try {
			return analyzer.analyzeForm(parameterValue, analysisBuilder);
		} finally {
			lexicalEnvironmentStack.push(currentLexicalEnvironment);
		}
	}

}
