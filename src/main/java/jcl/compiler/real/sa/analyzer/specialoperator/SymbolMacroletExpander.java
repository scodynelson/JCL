package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.SymbolMacroBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.SymbolMacroletStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
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
public class SymbolMacroletExpander extends MacroFunctionExpander<SymbolMacroletStruct> {

	private static final long serialVersionUID = 3878455475225336840L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the symbol-macrolet macro function and adds it to the special operator 'symbol-macrolet'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.SYMBOL_MACROLET.setMacroFunctionExpander(this);
	}

	@Override
	public SymbolMacroletStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + printedObject);
		}

		final SymbolMacroletEnvironment symbolMacroletEnvironment = new SymbolMacroletEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms, symbolMacroletEnvironment);
		final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();
		validateDeclares(declareElement);

		final List<SymbolMacroletStruct.SymbolMacroletVar> symbolMacroletVars
				= parametersAsJavaList.stream()
				                      .map(e -> getSymbolMacroletElementVar(e, declareElement, symbolMacroletEnvironment))
				                      .collect(Collectors.toList());

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, symbolMacroletEnvironment))
				           .collect(Collectors.toList());

		return new SymbolMacroletStruct(symbolMacroletVars, new PrognStruct(analyzedBodyForms), symbolMacroletEnvironment);
	}

	private static void validateDeclares(final DeclareStruct declareElement) {
		if (declareElement != null) {
			final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			if (!specialDeclarationElements.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed.");
			}
		}
	}

	private SymbolMacroletStruct.SymbolMacroletVar getSymbolMacroletElementVar(final LispStruct parameter, final DeclareStruct declareElement,
	                                                                           final SymbolMacroletEnvironment symbolMacroletEnvironment) {

		if (!(parameter instanceof ListStruct)) {
			final String printedParameter = printer.print(parameter);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be a list. Got: " + printedParameter);
		}

		final ListStruct listParameter = (ListStruct) parameter;
		final SymbolStruct<?> var = getSymbolMacroletParameterVar(listParameter);
		final LispStruct expansion = getSymbolMacroletParameterExpansion(listParameter, symbolMacroletEnvironment);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(symbolMacroletEnvironment);
		final int nextBindingsPosition = currentLambda.getNextParameterNumber();
		symbolMacroletEnvironment.setBindingsPosition(nextBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(nextBindingsPosition);
		final SymbolMacroBinding binding = new SymbolMacroBinding(var, allocation, T.INSTANCE, expansion);
		symbolMacroletEnvironment.addSymbolMacroBinding(binding);

		return new SymbolMacroletStruct.SymbolMacroletVar(var, expansion);
	}

	private SymbolStruct<?> getSymbolMacroletParameterVar(final ListStruct listParameter) {

		final int listParameterSize = listParameter.size();
		if (listParameterSize != 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must have only 2 elements. Got: " + listParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			final String printedObject = printer.print(listParameterFirst);
			throw new ProgramErrorException("SYMBOL-MACROLET: First element of parameter list must be a symbol. Got: " + printedObject);
		}

		final SymbolStruct<?> parameterVar = (SymbolStruct<?>) listParameterFirst;

		final boolean hasGlobalBinding = Environment.NULL.hasDynamicBinding(parameterVar);
		if (hasGlobalBinding) {
			final String printedObject = printer.print(parameterVar);
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list symbol must not be a dynamic binding in the global environment. Got: " + printedObject);
		}

		return parameterVar;
	}

	private LispStruct getSymbolMacroletParameterExpansion(final ListStruct listParameter, final SymbolMacroletEnvironment symbolMacroletEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = symbolMacroletEnvironment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
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
		final SymbolMacroletExpander rhs = (SymbolMacroletExpander) obj;
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
