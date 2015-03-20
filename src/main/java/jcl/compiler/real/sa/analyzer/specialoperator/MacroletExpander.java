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
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.MacroletStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroletExpander extends MacroFunctionExpander<MacroletStruct> {

	private static final long serialVersionUID = 920568167525914860L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MACROLET.setMacroFunctionExpander(this);
	}

	@Override
	public MacroletStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("MACROLET: Parameter list must be a list. Got: " + printedObject);
		}

		final MacroletEnvironment macroletEnvironment = new MacroletEnvironment(environment);

		final Stack<SymbolStruct<?>> functionNameStack = macroletEnvironment.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		try {
			final ListStruct innerFunctions = (ListStruct) second;
			final List<LispStruct> innerFunctionsAsJavaList = innerFunctions.getAsJavaList();
			functionNames = getFunctionNames(innerFunctionsAsJavaList);

			// Add function names BEFORE analyzing the functions. This is one of the differences between Flet and Labels/Macrolet.
			StackUtils.pushAll(functionNameStack, functionNames);

			final ListStruct formRestRest = formRest.getRest();
			final List<LispStruct> forms = formRestRest.getAsJavaList();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms, macroletEnvironment);
			final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

			final List<MacroletStruct.MacroletVar> macroletVars
					= innerFunctionsAsJavaList.stream()
					                          .map(e -> getMacroletVar(e, declareElement, macroletEnvironment))
					                          .collect(Collectors.toList());

			final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(specialDeclarationElement -> Environments.addDynamicVariableBinding(specialDeclarationElement, macroletEnvironment));

			final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
			final List<LispStruct> analyzedBodyForms
					= bodyForms.stream()
					           .map(e -> formAnalyzer.analyze(e, macroletEnvironment))
					           .collect(Collectors.toList());

			return new MacroletStruct(macroletVars, new PrognStruct(analyzedBodyForms), macroletEnvironment);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}
		}
	}

	private List<SymbolStruct<?>> getFunctionNames(final List<? extends LispStruct> functionDefinitions) {

		final List<SymbolStruct<?>> functionNames = new ArrayList<>(functionDefinitions.size());

		for (final LispStruct functionDefinition : functionDefinitions) {
			if (!(functionDefinition instanceof ListStruct)) {
				final String printedFunctionDefinition = printer.print(functionDefinition);
				throw new ProgramErrorException("MACROLET: Function parameter must be a list. Got: " + printedFunctionDefinition);
			}
			final ListStruct functionList = (ListStruct) functionDefinition;

			final LispStruct functionListFirst = functionList.getFirst();
			if (!(functionListFirst instanceof SymbolStruct)) {
				final String printedObject = printer.print(functionListFirst);
				throw new ProgramErrorException("MACROLET: First element of function parameter must be a symbol. Got: " + printedObject);
			}
			final SymbolStruct<?> functionName = (SymbolStruct<?>) functionListFirst;
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private MacroletStruct.MacroletVar getMacroletVar(final LispStruct functionDefinition, final DeclareStruct declareElement,
	                                                  final MacroletEnvironment macroletEnvironment) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct<?> functionName = (SymbolStruct<?>) functionList.getFirst();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, macroletEnvironment);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(macroletEnvironment);
		final int nextBindingsPosition = currentLambda.getNextParameterNumber();
		macroletEnvironment.setBindingsPosition(nextBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, functionName);

		final ParameterAllocation allocation = new ParameterAllocation(nextBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, allocation, T.INSTANCE, functionInitForm);
		if (isSpecial) {
			macroletEnvironment.addDynamicBinding(binding);
		} else {
			macroletEnvironment.addLexicalBinding(binding);
		}

		return new MacroletStruct.MacroletVar(functionName, functionInitForm);
	}

	private CompilerFunctionStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                            final MacroletEnvironment macroletEnvironment) {

		// TODO: This will be a MacroLambda, NOT a Lambda form!!!

		final int functionListParameterSize = functionListParameter.size();
		if (functionListParameterSize < 2) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments to function parameter: " + functionListParameterSize + ". Expected at least 2 arguments.");
		}

		final ListStruct functionListParameterRest = functionListParameter.getRest();

		final LispStruct functionName = functionListParameter.getFirst();
		final LispStruct lambdaList = functionListParameterRest.getFirst();
		final ListStruct body = functionListParameterRest.getRest();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct innerBlockListStruct = ListStruct.buildDottedList(SpecialOperator.BLOCK, functionName, body);
		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(SpecialOperator.LAMBDA, lambdaList, innerBlockListStruct);
		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(SpecialOperator.FUNCTION, innerLambdaListStruct);

		// Evaluate in the 'current' environment. This is one of the differences between Flet and Labels/Macrolet.
		return functionExpander.expand(innerFunctionListStruct, macroletEnvironment);
	}
}
