/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
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
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroLambdaExpander extends MacroFunctionExpander<MacroLambdaStruct> {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private MacroLambdaListParser macroLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the macro-lambda macro function and adds it to the special operator 'macro-lambda'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.MACRO_LAMBDA.setMacroFunctionExpander(this);
	}

	@Override
	public MacroLambdaStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 3) {
			throw new ProgramErrorException("MACRO LAMBDA: Incorrect number of arguments: " + formSize + ". Expected at least 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("MACRO LAMBDA: Macro name must be a symbol. Got: " + printedObject);
		}

		final SymbolStruct<?> macroName = (SymbolStruct) second;

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		if (!(third instanceof ListStruct)) {
			final String printedObject = printer.print(third);
			throw new ProgramErrorException("MACRO LAMBDA: Parameter list must be a list. Got: " + printedObject);
		}

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(environment);

		final ListStruct parameters = (ListStruct) third;

		final ListStruct formRestRestRest = formRestRest.getRest();
		final List<LispStruct> forms = formRestRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, lambdaEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(e -> new Binding(e, TType.INSTANCE))
		                   .forEach(lambdaEnvironment::addDynamicBinding);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String fileName;
		if (javaClassNameDeclaration == null) {
			final String className = "MacroLambda" + '_' + macroName.getName() + '_' + System.nanoTime();
			fileName = "jcl." + className;
		} else {
			fileName = javaClassNameDeclaration.getClassName();
		}

		final MacroLambdaListBindings parsedLambdaList = macroLambdaListParser.parseMacroLambdaList(lambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, lambdaEnvironment))
				           .collect(Collectors.toList());
		return new MacroLambdaStruct(fileName, macroName, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), lambdaEnvironment);
	}
}
