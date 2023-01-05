/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.sa.analyzer.lambdalistparser.MacroLambdaListParser;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.lambda.MacroLambdaStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class MacroLambdaExpander extends MacroFunctionExpander<MacroLambdaStruct> {

	public static final MacroLambdaExpander INSTANCE = new MacroLambdaExpander();

	private static final Pattern CLASS_SEPARATOR_PATTERN = Pattern.compile(".");

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MACRO_LAMBDA;
	}

	@Override
	public MacroLambdaStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MACRO-LAMBDA SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACRO-LAMBDA: Incorrect number of arguments: 0. Expected at least 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof final SymbolStruct macroName)) {
			throw new TypeErrorException("MACRO-LAMBDA: MACRO-NAME must be a Symbol. Got: " + first);
		}

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACRO-LAMBDA: Incorrect number of arguments: 1. Expected at least 2 arguments.");
		}
		final LispStruct second = iterator.next();

		if (!(second instanceof final ListStruct parameters)) {
			throw new TypeErrorException("MACRO-LAMBDA: PARAMETER-LIST must be a List. Got: " + second);
		}

		final Environment macroLambdaEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.toLispList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = DeclareExpander.INSTANCE.expand(fullDeclaration, macroLambdaEnvironment);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();

		// TODO: should go on this stack? or have another stack for macros?
		final Stack<SymbolStruct> functionNameStack = environment.getFunctionNameStack();
		functionNameStack.push(macroName);

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
//			className = javaClassName.replace('.', '/') + '_' + System.nanoTime();
			className = javaClassName.replace('.', '/');
		}

		final MacroLambdaList parsedLambdaList = MacroLambdaListParser.parseMacroLambdaList(macroLambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> FormAnalyzer.analyze(e, macroLambdaEnvironment))
				           .collect(Collectors.toList());
		return new MacroLambdaStruct(className, macroName, parsedLambdaList, bodyProcessingResult.getDocString(),
		                             new PrognStruct(analyzedBodyForms), macroLambdaEnvironment);
	}
}
