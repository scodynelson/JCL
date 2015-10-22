package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.LetStruct;
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
public class LetExpander extends MacroFunctionExpander<LetStruct> {

	private static final long serialVersionUID = 2933802423859476026L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the let macro function and adds it to the special operator 'let'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.LET.setMacroFunctionExpander(this);
	}

	@Override
	public LetStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LET: Parameter list must be a list. Got: " + printedObject);
		}

		final Environment letEnvironment = new Environment(environment);

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, letEnvironment);

		final List<LetStruct.LetVar> letVars
				= parametersAsJavaList.stream()
				                      .map(e -> getLetVar(e, declare, letEnvironment))
				                      .collect(Collectors.toList());

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(e -> new Binding(e, TType.INSTANCE))
		                   .forEach(letEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, letEnvironment))
				           .collect(Collectors.toList());

		return new LetStruct(letVars, new PrognStruct(analyzedBodyForms), letEnvironment);
	}

	private LetStruct.LetVar getLetVar(final LispStruct parameter, final DeclareStruct declare,
	                                   final Environment letEnvironment) {

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			final String printedParameter = printer.print(parameter);
			throw new ProgramErrorException("LET: Parameter must be a symbol or a list. Got: " + printedParameter);
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

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(Predicate.isEqual(var));

		final Binding binding = new Binding(var, TType.INSTANCE);
		if (isSpecial) {
			letEnvironment.addDynamicBinding(binding);
		} else {
			letEnvironment.addLexicalBinding(binding);
		}

		return new LetStruct.LetVar(var, initForm, isSpecial);
	}

	private SymbolStruct<?> getLetListParameterVar(final ListStruct listParameter) {
		final int listParameterSize = listParameter.size();
		if ((listParameterSize < 1) || (listParameterSize > 2)) {
			final String printedListParameter = printer.print(listParameter);
			throw new ProgramErrorException("LET: List parameter must have only 1 or 2 elements. Got: " + printedListParameter);
		}

		final LispStruct listParameterFirst = listParameter.getFirst();
		if (!(listParameterFirst instanceof SymbolStruct)) {
			final String printedObject = printer.print(listParameterFirst);
			throw new ProgramErrorException("LET: First element of list parameter must be a symbol. Got: " + printedObject);
		}
		return (SymbolStruct<?>) listParameterFirst;
	}

	private LispStruct getLetListParameterInitForm(final ListStruct listParameter, final Environment letEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = letEnvironment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
