/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import org.springframework.beans.factory.annotation.Autowired;

abstract class ClosureCreationExpander<V> extends MacroFunctionExpander<ClosureCreationStruct<V>> {

	@Autowired
	private FormAnalyzer formAnalyzer;

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
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // Closure Expander SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException(expanderName + ": Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			throw new TypeErrorException(expanderName + ": PARAMETER-LIST must be a List. Got: " + first);
		}
		final ListStruct parameters = (ListStruct) first;

		final Environment closureEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, closureEnvironment);

		final List<V> vars
				= parameters.stream()
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

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			throw new TypeErrorException(expanderName + ": PARAMETER must be a Symbol or a List. Got: " + parameter);
		}

		final SymbolStruct var;
		final LispStruct initForm;

		if (parameter instanceof ListStruct) {
			final ListStruct listParameter = (ListStruct) parameter;
			final Iterator<LispStruct> iterator = listParameter.iterator();

			if (!iterator.hasNext()) {
				throw new ProgramErrorException(expanderName + ": List parameter must have either 1 or 2 elements. Got: 0");
			}
			final LispStruct first = iterator.next();

			if (!(first instanceof SymbolStruct)) {
				throw new TypeErrorException(expanderName + ": First element of list parameter must be a Symbol. Got: " + first);
			}
			var = (SymbolStruct) first;

			if (iterator.hasNext()) {
				final LispStruct value = iterator.next();
				initForm = getListParameterInitForm(value, environment);

				if (iterator.hasNext()) {
					throw new ProgramErrorException(expanderName + ": List parameter must have either 1 or 2 elements. Got: 3");
				}
			} else {
				initForm = NILStruct.INSTANCE;
			}
		} else {
			var = (SymbolStruct) parameter;
			initForm = NILStruct.INSTANCE;
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

	protected abstract V getClosureCreationVar(SymbolStruct var, LispStruct initForm,
	                                           boolean isSpecial);

	protected abstract ClosureCreationStruct<V> getClosureCreationStruct(List<V> vars,
	                                                                     PrognStruct prognBody,
	                                                                     Environment environment);


	protected abstract LispStruct getListParameterInitForm(LispStruct parameterValue, Environment environment);
}
