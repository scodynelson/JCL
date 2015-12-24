/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class ClosureCreationExpander<V> extends MacroFunctionExpander<ClosureCreationStruct<V>> {

	private static final long serialVersionUID = -7834811764790179674L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	private final String expanderName;

	protected ClosureCreationExpander(final String expanderName) {
		this.expanderName = expanderName;
	}

	@Override
	public ClosureCreationStruct<V> expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, expanderName);

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct parameters = validator.validateObjectType(second, expanderName, "PARAMETER LIST", ListStruct.class);
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final Environment closureEnvironment = new Environment(environment);

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, closureEnvironment);

		final List<V> vars
				= parametersAsJavaList.stream()
				                      .map(e -> getVar(e, declare, closureEnvironment))
				                      .collect(Collectors.toList());

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(closureEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, closureEnvironment))
				           .collect(Collectors.toList());

		return getClosureCreationStruct(vars, new PrognStruct(analyzedBodyForms), closureEnvironment);
	}

	private V getVar(final LispStruct parameter, final DeclareStruct declare, final Environment environment) {

		validator.validateObjectTypes(parameter, expanderName, "PARAMETER", SymbolStruct.class, ListStruct.class);

		final SymbolStruct var;
		final LispStruct initForm;

		if (parameter instanceof ListStruct) {
			final ListStruct listParameter = (ListStruct) parameter;
			var = getListParameterVar(listParameter);
			initForm = getListParameterInitForm(listParameter, environment);
		} else {
			var = (SymbolStruct) parameter;
			initForm = NullStruct.INSTANCE;
		}

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(Predicate.isEqual(var));

		final Binding binding = new Binding(var);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		return getClosureCreationVar(var, initForm, isSpecial);
	}

	private SymbolStruct getListParameterVar(final ListStruct listParameter) {
		validator.validateListParameterSize(listParameter, 1, 2, expanderName);

		final LispStruct listParameterFirst = listParameter.getFirst();
		return validator.validateObjectType(listParameterFirst, expanderName, "First element of list parameter", SymbolStruct.class);
	}

	protected abstract V getClosureCreationVar(final SymbolStruct var, final LispStruct initForm,
	                                           final boolean isSpecial);

	protected abstract ClosureCreationStruct<V> getClosureCreationStruct(final List<V> vars,
	                                                                     final PrognStruct prognBody,
	                                                                     final Environment environment);


	protected abstract LispStruct getListParameterInitForm(final ListStruct listParameter, final Environment environment);
}
