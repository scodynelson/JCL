/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.sa.analyzer.lambdalistparser.MacroLambdaListParser;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.lambda.MacroLambdaStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroLambdaExpander extends MacroFunctionExpander<MacroLambdaStruct> {

	private static final long serialVersionUID = -3836341809506752205L;

	@Autowired
	private MacroLambdaListParser macroLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.MACRO_LAMBDA;
	}

	@Override
	public MacroLambdaStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 3, "MACRO-LAMBDA");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final SymbolStruct<?> macroName = validator.validateObjectType(second, "MACRO-LAMBDA", "MACRO NAME", SymbolStruct.class);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		final ListStruct parameters = validator.validateObjectType(third, "MACRO-LAMBDA", "PARAMETER LIST", ListStruct.class);

		final Environment macroLambdaEnvironment = new Environment(environment);

		final ListStruct formRestRestRest = formRestRest.getRest();
		final List<LispStruct> forms = formRestRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, macroLambdaEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(macroLambdaEnvironment::addDynamicBinding);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String fileName;
		if (javaClassNameDeclaration == null) {
			final String className = "MacroLambda" + '_' + macroName.getName() + '_' + System.nanoTime();
			fileName = "jcl." + className;
		} else {
			fileName = javaClassNameDeclaration.getClassName();
		}

		final MacroLambdaList parsedLambdaList = macroLambdaListParser.parseMacroLambdaList(macroLambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, macroLambdaEnvironment))
				           .collect(Collectors.toList());
		return new MacroLambdaStruct(fileName, macroName, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), macroLambdaEnvironment);
	}
}
