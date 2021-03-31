/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.DeclarationStructImpl;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;

@Log4j2
abstract class InnerLambdaExpander extends MacroFunctionExpander<InnerLambdaStruct> {

	private final String expanderName;

	protected InnerLambdaExpander(final String expanderName) {
		this.expanderName = expanderName;
	}

	@Override
	public InnerLambdaStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // InnerLambda Expander SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException(expanderName + ": Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			throw new TypeErrorException(expanderName + ": FUNCTION-LIST must be a List. Got: " + first);
		}
		final ListStruct innerLambdas = (ListStruct) first;

		final Environment innerLambdaEnvironment = new Environment(environment);

		final Stack<SymbolStruct> functionNameStack = environment.getFunctionNameStack();
		final List<SymbolStruct> functionNames = getFunctionNames(innerLambdas);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.toLispList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = DeclareExpander.INSTANCE.expand(fullDeclaration, innerLambdaEnvironment);

		return buildInnerLambda(innerLambdas, innerLambdaEnvironment, bodyProcessingResult, declare, functionNameStack, functionNames);
	}

	protected abstract InnerLambdaStruct buildInnerLambda(ListStruct innerLambdas,
	                                                      Environment innerLambdaEnvironment,
	                                                      BodyProcessingResult bodyProcessingResult,
	                                                      DeclareStruct declare,
	                                                      Stack<SymbolStruct> functionNameStack,
	                                                      List<SymbolStruct> functionNames);

	protected abstract ListStruct getInnerLambdaBody(ListStruct innerBlockListStruct, SymbolStruct functionNameSymbol,
	                                                 List<SymbolStruct> functionNames);

	protected abstract CompilerFunctionStruct expandBuiltInnerFunction(ListStruct innerFunctionListStruct,
	                                                                   Environment environment);

	protected List<InnerLambdaStruct.InnerLambdaVar> getVars(final ListStruct innerLambdas,
	                                                         final Environment innerLambdaEnvironment,
	                                                         final DeclareStruct declare,
	                                                         final List<SymbolStruct> functionNames) {
		return innerLambdas.stream()
		                   .map(e -> getVar(e, declare, innerLambdaEnvironment, functionNames))
		                   .collect(Collectors.toList());
	}

	protected InnerLambdaStruct getInnerLambda(final List<InnerLambdaStruct.InnerLambdaVar> vars,
	                                           final Environment innerLambdaEnvironment,
	                                           final BodyProcessingResult bodyProcessingResult) {

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> FormAnalyzer.analyze(e, innerLambdaEnvironment))
				           .collect(Collectors.toList());

		return new InnerLambdaStruct(vars, new PrognStruct(analyzedBodyForms), innerLambdaEnvironment);
	}

	private List<SymbolStruct> getFunctionNames(final ListStruct innerLambdas) {

		final List<SymbolStruct> functionNames = new ArrayList<>();

		for (final LispStruct functionDefinition : innerLambdas) {

			if (!(functionDefinition instanceof ListStruct)) {
				throw new TypeErrorException(expanderName + ": FUNCTION PARAMETER must be a List. Got: " + functionDefinition);
			}
			final ListStruct functionList = (ListStruct) functionDefinition;

			final LispStruct functionListFirst = functionList.car();
			if (!(functionListFirst instanceof SymbolStruct)) {
				throw new TypeErrorException(expanderName + ": First element of function parameter must be a Symbol. Got: " + functionListFirst);
			}
			final SymbolStruct functionName = (SymbolStruct) functionListFirst;

			if (functionNames.contains(functionName)) {
				log.warn("{}: Multiple bindings of {} in {} form.", expanderName, functionName.getName(), expanderName);
			}
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private InnerLambdaStruct.InnerLambdaVar getVar(final LispStruct functionDefinition, final DeclareStruct declare,
	                                                final Environment environment, final List<SymbolStruct> functionNames) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct functionName = (SymbolStruct) functionList.car();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, environment, functionNames);

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(functionName::eq);

		final Binding binding = new Binding(functionName);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		return new InnerLambdaStruct.InnerLambdaVar(functionName, functionInitForm, isSpecial);
	}

	private CompilerFunctionStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                            final Environment environment,
	                                                            final List<SymbolStruct> functionNames) {

		final Iterator<LispStruct> iterator = functionListParameter.iterator();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException(expanderName + ": Function parameter must have at least 2 elements. Got: 0");
		}
		final LispStruct functionName = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException(expanderName + ": Function parameter must have at least 2 elements. Got: 1");
		}
		final LispStruct lambdaList = iterator.next();

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final List<LispStruct> declares = bodyProcessingResult.getDeclares();
		final StringStruct docString = ObjectUtils.defaultIfNull(bodyProcessingResult.getDocString(), StringStruct.EMPTY_STRING);
		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct blockBody = ListStruct.toLispList(bodyForms);
		final ListStruct innerBlockListStruct = (ListStruct)
				ListStruct.toLispDottedList(SpecialOperatorStructImpl.BLOCK, functionName, blockBody);

		// NOTE: This will be a safe cast since we verify it is a symbol earlier
		final SymbolStruct functionNameSymbol = (SymbolStruct) functionName;

		final ListStruct innerLambdaBody = getInnerLambdaBody(innerBlockListStruct, functionNameSymbol, functionNames);

		final String functionNameString = functionNameSymbol.getName();
		final String properFunctionNameString = functionNameString.codePoints()
		                                                          .filter(Character::isJavaIdentifierPart)
		                                                          .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		                                                          .toString();

		final String paramName = "jcl." + expanderName + '_' + properFunctionNameString + "_Lambda_" + System.nanoTime();
		final StringStruct paramJavaClassName = StringStruct.toLispString(paramName);
		final ListStruct paramJavaClassNameDeclaration = ListStruct.toLispList(DeclarationStructImpl.JAVA_CLASS_NAME, paramJavaClassName);
		declares.add(paramJavaClassNameDeclaration);

		final ListStruct fullDeclaration = ListStruct.toLispList(declares);

		final ListStruct innerLambdaListStruct = ListStruct.toLispList(SpecialOperatorStructImpl.LAMBDA, lambdaList, fullDeclaration, docString, innerLambdaBody);
		final ListStruct innerFunctionListStruct = ListStruct.toLispList(SpecialOperatorStructImpl.FUNCTION, innerLambdaListStruct);

		return expandBuiltInnerFunction(innerFunctionListStruct, environment);
	}
}
