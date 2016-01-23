/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class InnerLambdaExpander extends MacroFunctionExpander<InnerLambdaStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(InnerLambdaExpander.class);

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	private final String expanderName;

	protected InnerLambdaExpander(final String expanderName) {
		this.expanderName = expanderName;
	}

	@Override
	public InnerLambdaStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, expanderName);

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct innerLambdas = validator.validateObjectType(second, expanderName, "FUNCTION LIST", ListStruct.class);
		final List<LispStruct> innerLambdasAsJavaList = innerLambdas.getAsJavaList();

		final Environment innerLambdaEnvironment = new Environment(environment);

		final Stack<SymbolStruct> functionNameStack = environment.getFunctionNameStack();
		final List<SymbolStruct> functionNames = getFunctionNames(innerLambdasAsJavaList);

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, innerLambdaEnvironment);

		return buildInnerLambda(innerLambdasAsJavaList, innerLambdaEnvironment, bodyProcessingResult, declare, functionNameStack, functionNames);
	}

	protected abstract InnerLambdaStruct buildInnerLambda(final List<LispStruct> innerLambdasAsJavaList,
	                                                      final Environment innerLambdaEnvironment,
	                                                      final BodyProcessingResult bodyProcessingResult,
	                                                      final DeclareStruct declare,
	                                                      final Stack<SymbolStruct> functionNameStack,
	                                                      final List<SymbolStruct> functionNames);

	protected abstract ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStruct functionNameSymbol,
	                                                 final List<SymbolStruct> functionNames);

	protected abstract CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct,
	                                                                   final Environment environment);

	protected List<InnerLambdaStruct.InnerLambdaVar> getVars(final List<LispStruct> innerLambdasAsJavaList,
	                                                         final Environment innerLambdaEnvironment,
	                                                         final DeclareStruct declare,
	                                                         final List<SymbolStruct> functionNames) {
		return innerLambdasAsJavaList.stream()
		                             .map(e -> getVar(e, declare, innerLambdaEnvironment, functionNames))
		                             .collect(Collectors.toList());
	}

	protected InnerLambdaStruct getInnerLambda(final List<InnerLambdaStruct.InnerLambdaVar> vars,
	                                           final Environment innerLambdaEnvironment,
	                                           final BodyProcessingResult bodyProcessingResult,
	                                           final DeclareStruct declare) {

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(innerLambdaEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, innerLambdaEnvironment))
				           .collect(Collectors.toList());

		return new InnerLambdaStruct(vars, new PrognStruct(analyzedBodyForms), innerLambdaEnvironment);
	}

	private List<SymbolStruct> getFunctionNames(final List<? extends LispStruct> functionDefinitions) {

		final List<SymbolStruct> functionNames = new ArrayList<>(functionDefinitions.size());

		for (final LispStruct functionDefinition : functionDefinitions) {
			final ListStruct functionList = validator.validateObjectType(functionDefinition, expanderName, "Function parameter", ListStruct.class);

			final LispStruct functionListFirst = functionList.getFirst();
			final SymbolStruct functionName = validator.validateObjectType(functionListFirst, expanderName, "First element of function parameter", SymbolStruct.class);

			if (functionNames.contains(functionName)) {
				LOGGER.warn("{}: Multiple bindings of {} in {} form.", expanderName, functionName.getName(), expanderName);
			}
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private InnerLambdaStruct.InnerLambdaVar getVar(final LispStruct functionDefinition, final DeclareStruct declare,
	                                                final Environment environment, final List<SymbolStruct> functionNames) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct functionName = (SymbolStruct) functionList.getFirst();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, environment, functionNames);

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(Predicate.isEqual(functionName));
		return new InnerLambdaStruct.InnerLambdaVar(functionName, functionInitForm, isSpecial);
	}

	private CompilerFunctionStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                            final Environment environment,
	                                                            final List<SymbolStruct> functionNames) {

		final int functionListParameterSize = functionListParameter.size();
		if (functionListParameterSize < 2) {
			throw new ProgramErrorException(expanderName + ": Incorrect number of arguments to function parameter: " + functionListParameterSize + ". Expected at least 2 arguments.");
		}

		final ListStruct functionListParameterRest = functionListParameter.getRest();

		final LispStruct functionName = functionListParameter.getFirst();
		final LispStruct lambdaList = functionListParameterRest.getFirst();
		final ListStruct body = functionListParameterRest.getRest();

		final List<LispStruct> forms = body.getAsJavaList();
		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final List<LispStruct> declares = bodyProcessingResult.getDeclares();
		final StringStruct docString = bodyProcessingResult.getDocString();
		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct blockBody = ListStruct.buildProperList(bodyForms);
		final ListStruct innerBlockListStruct = ListStruct.buildDottedList(SpecialOperatorStruct.BLOCK, functionName, blockBody);

		// NOTE: This will be a safe cast since we verify it is a symbol earlier
		final SymbolStruct functionNameSymbol = (SymbolStruct) functionName;

		final ListStruct innerLambdaBody = getInnerLambdaBody(innerBlockListStruct, functionNameSymbol, functionNames);

		final String functionNameString = functionNameSymbol.getName();
		final String properFunctionNameString = functionNameString.codePoints()
		                                                          .filter(Character::isJavaIdentifierPart)
		                                                          .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		                                                          .toString();

		final String paramName = "jcl." + expanderName + '_' + properFunctionNameString + "_Lambda_" + System.nanoTime();
		final StringStruct paramJavaClassName = new StringStruct(paramName);
		final ListStruct paramJavaClassNameDeclaration = ListStruct.buildProperList(DeclarationStruct.JAVA_CLASS_NAME, paramJavaClassName);
		declares.add(paramJavaClassNameDeclaration);

		final ListStruct fullDeclaration = ListStruct.buildProperList(declares);

		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(SpecialOperatorStruct.LAMBDA, lambdaList, fullDeclaration, docString, innerLambdaBody);
		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, innerLambdaListStruct);

		return expandBuiltInnerFunction(innerFunctionListStruct, environment);
	}
}
