package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends MacroFunctionExpander<LetStarStruct> {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.LET_STAR;
	}

	@Override
	public LetStarStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "LET*");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct parameters = validator.validateObjectType(second, "LET*", "PARAMETER LIST", ListStruct.class);
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final Environment letStarEnvironment = new Environment(environment);

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
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(letStarEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, letStarEnvironment))
				           .collect(Collectors.toList());

		return new LetStarStruct(letStarVars, new PrognStruct(analyzedBodyForms), letStarEnvironment);
	}

	private LetStarStruct.LetStarVar getLetStarVar(final LispStruct parameter, final DeclareStruct declare,
	                                               final Environment letStarEnvironment) {

		validator.validateObjectTypes(parameter, "LET*", "PARAMETER", SymbolStruct.class, ListStruct.class);

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

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(Predicate.isEqual(var));

		final Binding binding = new Binding(var);
		if (isSpecial) {
			letStarEnvironment.addDynamicBinding(binding);
		} else {
			letStarEnvironment.addLexicalBinding(binding);
		}

		return new LetStarStruct.LetStarVar(var, initForm, isSpecial);
	}

	private SymbolStruct<?> getLetStarListParameterVar(final ListStruct listParameter) {
		validator.validateListParameterSize(listParameter, 1, 2, "LET*");

		final LispStruct listParameterFirst = listParameter.getFirst();
		return validator.validateObjectType(listParameterFirst, "LET*", "First element of list parameter", SymbolStruct.class);
	}

	private LispStruct getLetStarListParameterInitForm(final ListStruct listParameter, final Environment letStarEnvironment) {

		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		return formAnalyzer.analyze(parameterValue, letStarEnvironment);
	}
}
