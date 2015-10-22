package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends MacroFunctionExpander<LetStarStruct> {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

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

		final BindingEnvironment letStarEnvironment = new BindingEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, letStarEnvironment);

		final List<LetStarStruct.LetStarVar> letStarVars
				= parametersAsJavaList.stream()
				                      .map(e -> getLetStarVar(e, declare, letStarEnvironment))
				                      .collect(Collectors.toList());

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.forEach(specialDeclaration -> Environments.addDynamicVariableBinding(specialDeclaration, letStarEnvironment));

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, letStarEnvironment))
				           .collect(Collectors.toList());

		return new LetStarStruct(letStarVars, new PrognStruct(analyzedBodyForms), letStarEnvironment);
	}

	private LetStarStruct.LetStarVar getLetStarVar(final LispStruct parameter, final DeclareStruct declare,
	                                               final BindingEnvironment letStarEnvironment) {

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

		final boolean isSpecial = Environments.isSpecial(declare, var);

		final Binding binding = new Binding(var, TType.INSTANCE);
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

	private LispStruct getLetStarListParameterInitForm(final ListStruct listParameter, final BindingEnvironment letStarEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		return formAnalyzer.analyze(parameterValue, letStarEnvironment);
	}
}
