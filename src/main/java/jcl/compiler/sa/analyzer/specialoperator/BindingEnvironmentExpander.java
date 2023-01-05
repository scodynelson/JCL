/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.BindingEnvironmentStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;

abstract class BindingEnvironmentExpander extends MacroFunctionExpander<BindingEnvironmentStruct> {

	private final String expanderName;

	protected BindingEnvironmentExpander(final String expanderName) {
		this.expanderName = expanderName;
	}

	@Override
	public BindingEnvironmentStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // Binding Environment Expander SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException(expanderName + ": Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof final ListStruct parameters)) {
			throw new TypeErrorException(expanderName + ": PARAMETER-LIST must be a List. Got: " + first);
		}

		final Environment bindingEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.toLispList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = DeclareExpander.INSTANCE.expand(fullDeclaration, bindingEnvironment);

		final List<BindingEnvironmentStruct.BindingVar> vars
				= parameters.stream()
				            .map(e -> getVar(e, declare, bindingEnvironment))
				            .collect(Collectors.toList());

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> FormAnalyzer.analyze(e, bindingEnvironment))
				           .collect(Collectors.toList());

		return getBindingEnvironmentStruct(vars, new PrognStruct(analyzedBodyForms), bindingEnvironment);
	}

	private BindingEnvironmentStruct.BindingVar getVar(final LispStruct parameter, final DeclareStruct declare,
	                                                   final Environment environment) {

		if (!(parameter instanceof SymbolStruct) && !(parameter instanceof ListStruct)) {
			throw new TypeErrorException(expanderName + ": PARAMETER must be a Symbol or a List. Got: " + parameter);
		}

		final SymbolStruct var;
		final LispStruct initForm;

		if (parameter instanceof final ListStruct listParameter) {
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
		                                 .anyMatch(var::eq);

		return new BindingEnvironmentStruct.BindingVar(var, initForm, isSpecial);
	}

	protected abstract BindingEnvironmentStruct getBindingEnvironmentStruct(
			List<BindingEnvironmentStruct.BindingVar> vars, PrognStruct prognBody, Environment environment
	);


	protected abstract LispStruct getListParameterInitForm(LispStruct parameterValue, Environment environment);
}
