package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.MacroletEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.MacroletStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroletExpander extends MacroFunctionExpander<MacroletStruct> {

	private static final long serialVersionUID = 920568167525914860L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MACROLET.setMacroFunctionExpander(this);
	}

	@Override
	public MacroletStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final MacroletEnvironment macroletEnvironment = new MacroletEnvironment(environment);

		final Stack<SymbolStruct<?>> functionNameStack = macroletEnvironment.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		try {
			final ListStruct innerFunctions = (ListStruct) second;
			final List<? extends LispStruct> innerFunctionsJavaList = innerFunctions.getAsJavaList();
			functionNames = getFunctionNames(innerFunctionsJavaList);

			// Add function names BEFORE analyzing the functions
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<LispStruct> bodyForms = inputRest.getRest().getAsJavaList();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(bodyForms, macroletEnvironment);
			final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

			final List<MacroletStruct.MacroletVar> macroletVars
					= innerFunctionsJavaList.stream()
					                        .map(e -> getMacroletVar(e, declareElement, macroletEnvironment))
					                        .collect(Collectors.toList());

			final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, macroletEnvironment));

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<LispStruct> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> formAnalyzer.analyze(e, macroletEnvironment))
					               .collect(Collectors.toList());

			return new MacroletStruct(macroletVars, analyzedBodyForms, macroletEnvironment);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}
		}
	}

	private List<SymbolStruct<?>> getFunctionNames(final List<? extends LispStruct> functionDefinitions) {

		final List<SymbolStruct<?>> functionNames = new ArrayList<>(functionDefinitions.size());

		for (final LispStruct currentFunctionDef : functionDefinitions) {
			if (!(currentFunctionDef instanceof ListStruct)) {
				throw new ProgramErrorException("MACROLET: Function parameter must be of type ListStruct. Got: " + currentFunctionDef);
			}
			final ListStruct functionList = (ListStruct) currentFunctionDef;

			final SymbolStruct<?> functionName = getFunctionListParameterName(functionList);
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private SymbolStruct<?> getFunctionListParameterName(final ListStruct functionListParameter) {
		final LispStruct functionListParameterFirst = functionListParameter.getFirst();
		if (!(functionListParameterFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException("MACROLET: Function parameter first element value must be of type SymbolStruct. Got: " + functionListParameterFirst);
		}
		return (SymbolStruct<?>) functionListParameterFirst;
	}

	private MacroletStruct.MacroletVar getMacroletVar(final LispStruct functionParameter,
	                                                  final DeclareStruct declareElement,
	                                                  final MacroletEnvironment macroletEnvironment) {

		if (!(functionParameter instanceof ListStruct)) {
			throw new ProgramErrorException("MACROLET: Function parameter must be of type ListStruct. Got: " + functionParameter);
		}

		final ListStruct functionListParameter = (ListStruct) functionParameter;
		final SymbolStruct<?> functionName = getFunctionListParameterName(functionListParameter);
		final LispStruct functionInitForm = getFunctionParameterInitForm(functionListParameter, macroletEnvironment);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(macroletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		macroletEnvironment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = isSpecial(declareElement, functionName);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, allocation, T.INSTANCE, functionInitForm);
		if (isSpecial) {
			macroletEnvironment.addDynamicBinding(binding);
		} else {
			macroletEnvironment.addLexicalBinding(binding);
		}

		return new MacroletStruct.MacroletVar(functionName, functionInitForm);
	}

	private LispStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                final MacroletEnvironment macroletEnvironment) {

		// TODO: This will be a MacroLambda, NOT a Lambda form!!!

		final int functionListParameterSize = functionListParameter.size();
		if (functionListParameterSize < 2) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments to function parameter: " + functionListParameterSize + ". Expected at least 2 arguments.");
		}

		final LispStruct functionName = functionListParameter.getFirst();

		final ListStruct functionListParameterRest = functionListParameter.getRest();

		final LispStruct lambdaList = functionListParameterRest.getFirst();

		final List<LispStruct> body = functionListParameterRest.getRest().getAsJavaList();

		final List<LispStruct> innerBlock = new ArrayList<>();
		innerBlock.add(SpecialOperator.BLOCK);
		innerBlock.add(functionName);
		innerBlock.addAll(body);

		final ListStruct innerBlockListStruct = ListStruct.buildProperList(innerBlock);

		final List<LispStruct> innerLambda = new ArrayList<>();
		innerLambda.add(SpecialOperator.LAMBDA);
		innerLambda.add(lambdaList);
		innerLambda.add(innerBlockListStruct);

		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(innerLambda);

		final List<LispStruct> innerFunction = new ArrayList<>();
		innerFunction.add(SpecialOperator.FUNCTION);
		innerFunction.add(innerLambdaListStruct);

		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(innerFunction);

		// Evaluate in the outer environment. This is one of the differences between Flet and Labels.
		final Environment parentEnvironment = macroletEnvironment.getParent();
		return formAnalyzer.analyze(innerFunctionListStruct, parentEnvironment);
	}

	private static boolean isSpecial(final DeclareStruct declareElement, final SymbolStruct<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationStruct specialDeclarationElement : specialDeclarationElements) {
			final SymbolStruct<?> specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}

	private static void addDynamicVariableBinding(final SpecialDeclarationStruct specialDeclarationElement,
	                                              final MacroletEnvironment macroletEnvironment) {

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(macroletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		macroletEnvironment.setBindingsPosition(newBindingsPosition);

		final SymbolStruct<?> var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(macroletEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		macroletEnvironment.addDynamicBinding(binding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
