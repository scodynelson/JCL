/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.sa.analyzer.lambdalistparser.MacroLambdaListParser;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.struct.specialoperator.lambda.MacroLambdaStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroLambdaExpander extends MacroFunctionExpander<MacroLambdaStruct> {

	private static final Pattern CLASS_SEPARATOR_PATTERN = Pattern.compile(".");

	@Autowired
	private MacroLambdaListParser macroLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.MACRO_LAMBDA;
	}

	@Override
	public MacroLambdaStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // Closure Expander SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACRO-LAMBDA: Incorrect number of arguments: 0. Expected at least 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new TypeErrorException("MACRO-LAMBDA: MACRO-NAME must be a Symbol. Got: " + first);
		}
		final SymbolStruct macroName = (SymbolStruct) first;

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACRO-LAMBDA: Incorrect number of arguments: 1. Expected at least 2 arguments.");
		}
		final LispStruct second = iterator.next();

		if (!(second instanceof ListStruct)) {
			throw new TypeErrorException("MACRO-LAMBDA: PARAMETER-LIST must be a List. Got: " + second);
		}
		final ListStruct parameters = (ListStruct) second;

		final Environment macroLambdaEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, macroLambdaEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(macroLambdaEnvironment::addDynamicBinding);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String className;
		if (javaClassNameDeclaration == null) {
			final String name = macroName.getName().replace('-', '_');
			final String realName = name.chars()
			                            .filter(Character::isJavaIdentifierPart)
			                            .mapToObj(e -> (char) e)
			                            .map(String::valueOf)
			                            .collect(Collectors.joining());
			final String macroLambdaClassName = realName + '_' + "MacroLambda" + '_' + System.nanoTime();
			className = "jcl/" + macroLambdaClassName;
		} else {
			final String javaClassName = javaClassNameDeclaration.getClassName();
			if (javaClassName.startsWith(".") || javaClassName.endsWith(".")) {
				throw new ErrorException("Invalid class definition for Macro Lambda: " + javaClassName);
			}

			final int lastSeparator = javaClassName.lastIndexOf('.');
			final String packageNameString = javaClassName.substring(0, lastSeparator);

			final boolean hasInvalidPackageName =
					!Arrays.stream(CLASS_SEPARATOR_PATTERN.split(packageNameString))
					       .map(CharSequence::chars)
					       .allMatch(packageStringChars -> packageStringChars.allMatch(Character::isJavaIdentifierPart));
			if (hasInvalidPackageName) {
				throw new ErrorException("Invalid package name for Macro Lambda: " + packageNameString);
			}

			final String classNameString = javaClassName.substring(lastSeparator + 1);
			final boolean hasInvalidClassName = !Character.isJavaIdentifierStart(classNameString.charAt(0)) || !classNameString.chars().allMatch(Character::isJavaIdentifierPart);
			if (hasInvalidClassName) {
				throw new ErrorException("Invalid class name for Macro Lambda: " + classNameString);
			}

			// TODO: Remove System.nanoTime() from here, since this breaks JAR loading. But we need it for now.
			className = javaClassName.replace('.', '/') + '_' + System.nanoTime();
		}

		final MacroLambdaList parsedLambdaList = macroLambdaListParser.parseMacroLambdaList(macroLambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, macroLambdaEnvironment))
				           .collect(Collectors.toList());
		return new MacroLambdaStruct(className, macroName, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), macroLambdaEnvironment);
	}
}