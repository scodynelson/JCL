package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LetStarEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends MacroFunctionExpander<LetStarStruct> {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the let* macro function and adds it to the special operator 'let*'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.LET_STAR.setMacroFunctionExpander(this);
	}

	@Override
	public LetStarStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LET*: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LET*: Parameter list must be a list. Got: " + printedObject);
		}

		final LetStarEnvironment letStarEnvironment = new LetStarEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms, letStarEnvironment);
		final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

		final List<LetStarStruct.LetStarVar> letStarVars
				= parametersAsJavaList.stream()
				                      .map(e -> getLetStarVar(e, declareElement, letStarEnvironment))
				                      .collect(Collectors.toList());

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		specialDeclarationElements.forEach(e -> Environments.addDynamicVariableBinding(e, letStarEnvironment));

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, letStarEnvironment))
				           .collect(Collectors.toList());

		return new LetStarStruct(letStarVars, new PrognStruct(analyzedBodyForms), letStarEnvironment);
	}

	private LetStarStruct.LetStarVar getLetStarVar(final LispStruct parameter, final DeclareStruct declareElement,
	                                               final LetStarEnvironment letStarEnvironment) {

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			final String printedParameter = printer.print(parameter);
			throw new ProgramErrorException("LET*: Parameter must be a symbol or a list. Got: " + printedParameter);
		}

		final SymbolStruct<?> var;
		final LispStruct initForm;

		if (parameter instanceof ListStruct) {
			final ListStruct listParameter = (ListStruct) parameter;
			var = getLetStarListParameterVar(listParameter);
			initForm = getLetStarListParameterInitForm(listParameter, letStarEnvironment);
		} else {
			var = (SymbolStruct<?>) parameter;
			initForm = NullStruct.INSTANCE;
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(letStarEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		letStarEnvironment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, var);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, initForm);
		if (isSpecial) {
			letStarEnvironment.addDynamicBinding(binding);
		} else {
			letStarEnvironment.addLexicalBinding(binding);
		}

		return new LetStarStruct.LetStarVar(var, initForm, isSpecial);
	}

	private SymbolStruct<?> getLetStarListParameterVar(final ListStruct listParameter) {
		final int listParameterSize = listParameter.size();
		if ((listParameterSize < 1) || (listParameterSize > 2)) {
			final String printedListParameter = printer.print(listParameter);
			throw new ProgramErrorException("LET*: List parameter must have only 1 or 2 elements. Got: " + printedListParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			final String printedObject = printer.print(listParameterFirst);
			throw new ProgramErrorException("LET*: First element of list parameter must be a symbol. Got: " + printedObject);
		}
		return (SymbolStruct<?>) listParameterFirst;
	}

	private LispStruct getLetStarListParameterInitForm(final ListStruct listParameter, final LetStarEnvironment letStarEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		return formAnalyzer.analyze(parameterValue, letStarEnvironment);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(bodyWithDeclaresAnalyzer)
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
		final LetStarExpander rhs = (LetStarExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(bodyWithDeclaresAnalyzer, rhs.bodyWithDeclaresAnalyzer)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(bodyWithDeclaresAnalyzer)
		                                                                .append(printer)
		                                                                .toString();
	}
}
