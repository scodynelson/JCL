package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetExpander extends MacroFunctionExpander<LetStruct> {

	private static final long serialVersionUID = 2933802423859476026L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LET.setMacroFunctionExpander(this);
	}

	@Override
	public LetStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LET: Parameter list must be of type ListStruct. Got: " + printedObject);
		}

		final LetEnvironment letEnvironment = new LetEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> bodyForms = inputRest.getRest().getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(bodyForms, letEnvironment);
		final DeclareStruct declareElement = bodyProcessingResult.getDeclareElement();

		final List<? extends LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final List<LetStruct.LetVar> letVars
				= parametersAsJavaList.stream()
				                      .map(e -> getLetVar(e, declareElement, letEnvironment))
				                      .collect(Collectors.toList());

		final List<SpecialDeclarationStruct> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		specialDeclarationElements.forEach(e -> Environments.addDynamicVariableBinding(e, letEnvironment));

		final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

		final List<LispStruct> analyzedBodyForms
				= realBodyForms.stream()
				               .map(e -> formAnalyzer.analyze(e, letEnvironment))
				               .collect(Collectors.toList());

		return new LetStruct(letVars, analyzedBodyForms, letEnvironment);
	}

	private LetStruct.LetVar getLetVar(final LispStruct parameter,
	                                   final DeclareStruct declareElement,
	                                   final LetEnvironment letEnvironment) {

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + parameter);
		}

		final SymbolStruct<?> var;
		final LispStruct initForm;

		if (parameter instanceof ListStruct) {
			final ListStruct listParameter = (ListStruct) parameter;
			var = getLetListParameterVar(listParameter);
			initForm = getLetListParameterInitForm(listParameter, letEnvironment);
		} else {
			var = (SymbolStruct<?>) parameter;
			initForm = NullStruct.INSTANCE;
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(letEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		letEnvironment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, var);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, initForm);
		if (isSpecial) {
			letEnvironment.addDynamicBinding(binding);
		} else {
			letEnvironment.addLexicalBinding(binding);
		}

		return new LetStruct.LetVar(var, initForm);
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
		return (SymbolStruct<?>) listParameterFirst;
	}

	private LispStruct getLetListParameterInitForm(final ListStruct listParameter,
	                                               final LetEnvironment letEnvironment) {

		final LispStruct parameterValue = listParameter.getRest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = letEnvironment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
