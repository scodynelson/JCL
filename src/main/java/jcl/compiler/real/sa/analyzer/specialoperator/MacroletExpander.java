package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.MacroletEnvironment;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.MacroletStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import jcl.types.TType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroletExpander extends MacroFunctionExpander<MacroletStruct> {

	private static final long serialVersionUID = 920568167525914860L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the macrolet macro function and adds it to the special operator 'macrolet'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.MACROLET.setMacroFunctionExpander(this);
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

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

			final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
			final DeclareStruct declare = declareExpander.expand(fullDeclaration, macroletEnvironment);

			final List<MacroletStruct.MacroletVar> macroletVars
					= innerFunctionsAsJavaList.stream()
					                          .map(e -> getMacroletVar(e, declare, macroletEnvironment))
					                          .collect(Collectors.toList());

			final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
			specialDeclarations.forEach(specialDeclaration -> Environments.addDynamicVariableBinding(specialDeclaration, macroletEnvironment));

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

	private MacroletStruct.MacroletVar getMacroletVar(final LispStruct functionDefinition, final DeclareStruct declare,
	                                                  final MacroletEnvironment macroletEnvironment) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct<?> functionName = (SymbolStruct<?>) functionList.getFirst();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, macroletEnvironment);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(macroletEnvironment);
		final int nextBindingsPosition = currentLambda.getNextParameterNumber();
		macroletEnvironment.setBindingsPosition(nextBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declare, functionName);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, TType.INSTANCE, functionInitForm);
		if (isSpecial) {
			macroletEnvironment.addDynamicBinding(binding);
		} else {
			macroletEnvironment.addLexicalBinding(binding);
		}

		return new MacroletStruct.MacroletVar(functionName, functionInitForm, isSpecial);
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

		final List<LispStruct> forms = body.getAsJavaList();
		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final List<LispStruct> declares = bodyProcessingResult.getDeclares();
		final StringStruct docString = bodyProcessingResult.getDocString();
		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct blockBody = ListStruct.buildProperList(bodyForms);
		final ListStruct innerBlockListStruct = ListStruct.buildDottedList(SpecialOperatorStruct.BLOCK, functionName, blockBody);

		// NOTE: This will be a safe cast since we verify it is a symbol earlier
		final SymbolStruct<?> functionNameSymbol = (SymbolStruct) functionName;

		final String functionNameString = functionNameSymbol.getName();
		final String properFunctionNameString = functionNameString.codePoints()
		                                                          .filter(Character::isJavaIdentifierPart)
		                                                          .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		                                                          .toString();

		final String macroletParamName = "jcl.MACROLET_" + properFunctionNameString + "_MacroLambda_" + System.nanoTime();
		final StringStruct macroletParamJavaClassName = new StringStruct(macroletParamName);
		final ListStruct macroletParamJavaClassNameDeclaration = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, macroletParamJavaClassName);
		declares.add(macroletParamJavaClassNameDeclaration);

		final ListStruct fullDeclaration = ListStruct.buildProperList(declares);

		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(SpecialOperatorStruct.LAMBDA, lambdaList, fullDeclaration, docString, innerBlockListStruct);
		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, innerLambdaListStruct);

		// Evaluate in the 'current' environment. This is one of the differences between Flet and Labels/Macrolet.
		return functionExpander.expand(innerFunctionListStruct, macroletEnvironment);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(declareExpander)
		                            .append(bodyWithDeclaresAnalyzer)
		                            .append(bodyWithDeclaresAndDocStringAnalyzer)
		                            .append(functionExpander)
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final MacroletExpander rhs = (MacroletExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(declareExpander, rhs.declareExpander)
		                          .append(bodyWithDeclaresAnalyzer, rhs.bodyWithDeclaresAnalyzer)
		                          .append(bodyWithDeclaresAndDocStringAnalyzer, rhs.bodyWithDeclaresAndDocStringAnalyzer)
		                          .append(functionExpander, rhs.functionExpander)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(declareExpander)
		                                                                .append(bodyWithDeclaresAnalyzer)
		                                                                .append(bodyWithDeclaresAndDocStringAnalyzer)
		                                                                .append(functionExpander)
		                                                                .append(printer)
		                                                                .toString();
	}
}
