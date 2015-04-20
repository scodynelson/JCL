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
import jcl.compiler.real.environment.LabelsEnvironment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LabelsStruct;
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
public class LabelsExpander extends MacroFunctionExpander<LabelsStruct> {

	private static final long serialVersionUID = -3698985413039911540L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the labels macro function and adds it to the special operator 'labels'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.LABELS.setMacroFunctionExpander(this);
	}

	@Override
	public LabelsStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LABELS: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LABELS: Parameter list must be a list. Got: " + printedObject);
		}

		final LabelsEnvironment labelsEnvironment = new LabelsEnvironment(environment);

		final Stack<SymbolStruct<?>> functionNameStack = environment.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		try {
			final ListStruct innerFunctions = (ListStruct) second;
			final List<LispStruct> innerFunctionsAsJavaList = innerFunctions.getAsJavaList();
			functionNames = getFunctionNames(innerFunctionsAsJavaList);

			// Add function names BEFORE analyzing the functions. This is one of the differences between Flet and Labels/Macrolet.
			StackUtils.pushAll(functionNameStack, functionNames);

			final ListStruct formRestRest = formRest.getRest();
			final List<LispStruct> forms = formRestRest.getAsJavaList();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms, labelsEnvironment);
			final DeclareStruct declare = bodyProcessingResult.getDeclareElement();

			final List<LabelsStruct.LabelsVar> labelsVars
					= innerFunctionsAsJavaList.stream()
					                          .map(e -> getLabelsVar(e, declare, labelsEnvironment))
					                          .collect(Collectors.toList());

			final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
			specialDeclarations.forEach(specialDeclaration -> Environments.addDynamicVariableBinding(specialDeclaration, labelsEnvironment));

			final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
			final List<LispStruct> analyzedBodyForms
					= bodyForms.stream()
					           .map(e -> formAnalyzer.analyze(e, labelsEnvironment))
					           .collect(Collectors.toList());

			return new LabelsStruct(labelsVars, new PrognStruct(analyzedBodyForms), labelsEnvironment);
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
				throw new ProgramErrorException("LABELS: Function parameter must be a list. Got: " + printedFunctionDefinition);
			}
			final ListStruct functionList = (ListStruct) functionDefinition;

			final LispStruct functionListFirst = functionList.getFirst();
			if (!(functionListFirst instanceof SymbolStruct)) {
				final String printedObject = printer.print(functionListFirst);
				throw new ProgramErrorException("LABELS: First element of function parameter must be a symbol. Got: " + printedObject);
			}
			final SymbolStruct<?> functionName = (SymbolStruct<?>) functionListFirst;
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private LabelsStruct.LabelsVar getLabelsVar(final LispStruct functionDefinition, final DeclareStruct declare,
	                                            final LabelsEnvironment labelsEnvironment) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct<?> functionName = (SymbolStruct<?>) functionList.getFirst();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, labelsEnvironment);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(labelsEnvironment);
		final int nextBindingsPosition = currentLambda.getNextParameterNumber();
		labelsEnvironment.setBindingsPosition(nextBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declare, functionName);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, TType.INSTANCE, functionInitForm);
		labelsEnvironment.addFunctionBinding(binding);

		return new LabelsStruct.LabelsVar(functionName, functionInitForm, isSpecial);
	}

	private CompilerFunctionStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                            final LabelsEnvironment labelsEnvironment) {

		final int functionListParameterSize = functionListParameter.size();
		if (functionListParameterSize < 2) {
			throw new ProgramErrorException("LABELS: Incorrect number of arguments to function parameter: " + functionListParameterSize + ". Expected at least 2 arguments.");
		}

		final ListStruct functionListParameterRest = functionListParameter.getRest();

		final LispStruct functionName = functionListParameter.getFirst();
		final LispStruct lambdaList = functionListParameterRest.getFirst();
		final ListStruct body = functionListParameterRest.getRest();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct innerBlockListStruct = ListStruct.buildDottedList(SpecialOperatorStruct.BLOCK, functionName, body);

		// NOTE: This will be a safe cast since we verify it is a symbol earlier
		final SymbolStruct<?> functionNameSymbol = (SymbolStruct) functionName;

		final String functionNameString = functionNameSymbol.getName();
		final String properFunctionNameString = functionNameString.codePoints()
		                                                          .filter(Character::isJavaIdentifierPart)
		                                                          .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		                                                          .toString();

		final String fletParamName = "jcl.LABELS_" + properFunctionNameString + "_Lambda_" + System.nanoTime();
		final StringStruct fletParamJavaClassName = new StringStruct(fletParamName);
		final ListStruct fletParamJavaClassNameDeclaration = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, fletParamJavaClassName);
		final ListStruct innerDeclareListStruct = ListStruct.buildProperList(SpecialOperatorStruct.DECLARE, fletParamJavaClassNameDeclaration);

		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(SpecialOperatorStruct.LAMBDA, lambdaList, innerDeclareListStruct, innerBlockListStruct);
		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, innerLambdaListStruct);

		// Evaluate in the 'current' environment. This is one of the differences between Flet and Labels/Macrolet.
		return functionExpander.expand(innerFunctionListStruct, labelsEnvironment);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(bodyWithDeclaresAnalyzer)
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
		final LabelsExpander rhs = (LabelsExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(bodyWithDeclaresAnalyzer, rhs.bodyWithDeclaresAnalyzer)
		                          .append(functionExpander, rhs.functionExpander)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(bodyWithDeclaresAnalyzer)
		                                                                .append(functionExpander)
		                                                                .append(printer)
		                                                                .toString();
	}
}
