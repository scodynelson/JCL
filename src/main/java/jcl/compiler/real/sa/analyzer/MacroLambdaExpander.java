/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.lambdalistparser.MacroLambdaListParser;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroLambdaExpander extends MacroFunctionExpander<LambdaStruct> {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private MacroLambdaListParser macroLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the lambda macro function and adds it to the special operator 'lambda'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.LAMBDA.setMacroFunctionExpander(this);
	}

	@Override
	public LambdaStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LAMBDA: Parameter list must be a list. Got: " + printedObject);
		}

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(environment);

		final ListStruct parameters = (ListStruct) second;

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms, lambdaEnvironment);
		final DeclareStruct declare = bodyProcessingResult.getDeclareElement();

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.forEach(specialDeclaration -> Environments.addDynamicVariableBinding(specialDeclaration, lambdaEnvironment));

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String fileName;
		if (javaClassNameDeclaration == null) {
			final String className = "Lambda" + '_' + System.nanoTime();
			fileName = "jcl." + className;
		} else {
			fileName = javaClassNameDeclaration.getClassName();
		}

		final OrdinaryLambdaListBindings parsedLambdaList = macroLambdaListParser.parseMacroLambdaList(lambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> newLambdaBodyForms = getNewStartingLambdaBody(parsedLambdaList, bodyForms);

		final List<LispStruct> analyzedBodyForms
				= newLambdaBodyForms.stream()
				                    .map(e -> formAnalyzer.analyze(e, lambdaEnvironment))
				                    .collect(Collectors.toList());
		return new LambdaStruct(fileName, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), lambdaEnvironment);
	}

	private static List<LispStruct> getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList,
	                                                         final List<LispStruct> bodyForms) {

		final List<LispStruct> bodyFormsWithInitFormSetqs = new ArrayList<>();

		final List<ListStruct> initFormIfSetqs = getInitFormIfSetqs(parsedLambdaList);
		bodyFormsWithInitFormSetqs.addAll(initFormIfSetqs);
		bodyFormsWithInitFormSetqs.addAll(bodyForms);

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			return bodyFormsWithInitFormSetqs;
		}

		final List<LispStruct> bodyWithAuxBindings = new ArrayList<>();
		bodyWithAuxBindings.add(SpecialOperatorStruct.LET_STAR);

		final List<LispStruct> auxLetStarVars
				= auxBindings.stream()
				             .map(e -> ListStruct.buildProperList(e.getSymbolStruct(), e.getInitForm()))
				             .collect(Collectors.toList());

		final ListStruct auxLetStarParams = ListStruct.buildProperList(auxLetStarVars);
		bodyWithAuxBindings.add(auxLetStarParams);

		bodyWithAuxBindings.addAll(bodyFormsWithInitFormSetqs);

		final ListStruct newLambdaBody = ListStruct.buildProperList(bodyWithAuxBindings);
		// NOTE: We are making sure to wrap this in a list for the processing of the body.
		//       Yes, it is a body of one element: a let* element
		return Collections.singletonList(newLambdaBody);
	}

	private static List<ListStruct> getInitFormIfSetqs(final OrdinaryLambdaListBindings parsedLambdaList) {

		final List<ListStruct> initFormIfSetqs = new ArrayList<>();

		final List<OptionalBinding> optionalBindings = parsedLambdaList.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();

			final SymbolStruct<?> optionalVar = optionalBinding.getSymbolStruct();
			final LispStruct optionalInitForm = optionalBinding.getInitForm();
			final ListStruct initFormSetq
					= ListStruct.buildProperList(SpecialOperatorStruct.SETQ, optionalVar, optionalInitForm);

			final SymbolStruct<?> suppliedPVar = suppliedPBinding.getSymbolStruct();
			final ListStruct initFormIfSetq
					= ListStruct.buildProperList(SpecialOperatorStruct.IF, suppliedPVar, NullStruct.INSTANCE, initFormSetq);

			initFormIfSetqs.add(initFormIfSetq);
		}

		final List<KeyBinding> keyBindings = parsedLambdaList.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();

			final SymbolStruct<?> keyVar = keyBinding.getSymbolStruct();
			final LispStruct keyInitForm = keyBinding.getInitForm();
			final ListStruct initFormSetq
					= ListStruct.buildProperList(SpecialOperatorStruct.SETQ, keyVar, keyInitForm);

			final ListStruct initFormIfSetq;
			if (suppliedPBinding == null) {
				initFormIfSetq = initFormSetq;
			} else {
				final SymbolStruct<?> suppliedPVar = suppliedPBinding.getSymbolStruct();
				initFormIfSetq
						= ListStruct.buildProperList(SpecialOperatorStruct.IF, suppliedPVar, NullStruct.INSTANCE, initFormSetq);
			}

			initFormIfSetqs.add(initFormIfSetq);
		}

		return initFormIfSetqs;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(bodyWithDeclaresAndDocStringAnalyzer)
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
		final MacroLambdaExpander rhs = (MacroLambdaExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(bodyWithDeclaresAndDocStringAnalyzer, rhs.bodyWithDeclaresAndDocStringAnalyzer)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(bodyWithDeclaresAndDocStringAnalyzer)
		                                                                .append(printer)
		                                                                .toString();
	}
}
