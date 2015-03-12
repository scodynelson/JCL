package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.struct.specialoperator.SymbolMacroletStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolMacroletExpander extends MacroFunctionExpander<SymbolMacroletStruct> {

	private static final long serialVersionUID = 3878455475225336840L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.SYMBOL_MACROLET.setMacroFunctionExpander(this);
	}

	@Override
	public SymbolMacroletStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
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
			final List<LispStruct> bodyForms = inputRest.getRest().getAsJavaList();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(bodyForms, analysisBuilder);
			final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();
			validateDeclares(declareElement);

			final List<? extends LispStruct> parametersAsJavaList = parameters.getAsJavaList();

			final List<SymbolMacroletStruct.SymbolMacroletElementVar> symbolMacroletVars
					= parametersAsJavaList.stream()
					                      .map(e -> getSymbolMacroletElementVar(e, declareElement, analysisBuilder, symbolMacroletEnvironment, environmentStack))
					                      .collect(Collectors.toList());

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<LispStruct> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> formAnalyzer.analyze(e, analysisBuilder))
					               .collect(Collectors.toList());

			return new SymbolMacroletStruct(symbolMacroletVars, analyzedBodyForms, symbolMacroletEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static void validateDeclares(final DeclareStruct declareElement) {
		if (declareElement != null) {
			final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			if (!specialDeclarationElements.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed. Got: " + specialDeclarationElements);
			}
		}
	}

	private SymbolMacroletStruct.SymbolMacroletElementVar getSymbolMacroletElementVar(final LispStruct parameter,
	                                                                                  final DeclareStruct declareElement,
	                                                                                  final AnalysisBuilder analysisBuilder,
	                                                                                  final SymbolMacroletEnvironment symbolMacroletEnvironment,
	                                                                                  final EnvironmentStack environmentStack) {

		if (!(parameter instanceof ListStruct)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + parameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final SymbolStruct<?> var = getSymbolMacroletParameterVar(listParameter, environmentStack);
		final LispStruct expansion = getSymbolMacroletParameterExpansion(listParameter, analysisBuilder, environmentStack);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(symbolMacroletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, expansion);
		symbolMacroletEnvironment.addLexicalBinding(binding);

		return new SymbolMacroletStruct.SymbolMacroletElementVar(var, expansion);
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

		final SymbolStruct<?> parameterVar = (SymbolStruct<?>) listParameterFirst;

		final Environment globalEnvironment = lexicalEnvironmentStack.firstElement();
		final boolean hasGlobalBinding = globalEnvironment.hasLexicalBinding(parameterVar);
		if (hasGlobalBinding) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element symbol must not exist in the global environment.");
		}

		return parameterVar;
	}

	private LispStruct getSymbolMacroletParameterExpansion(final ListStruct listParameter,
	                                                       final AnalysisBuilder analysisBuilder,
	                                                       final EnvironmentStack environmentStack) {

		final LispStruct parameterValue = listParameter.getRest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment currentEnvironment = environmentStack.pop();

		try {
			return formAnalyzer.analyze(parameterValue, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
