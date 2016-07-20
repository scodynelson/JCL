package jcl.compiler.sa.analyzer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.sa.analyzer.lambdalistparser.OrdinaryLambdaListParser;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.LispNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaExpander extends MacroFunctionExpander<LambdaStruct> {

	private static final Pattern CLASS_SEPARATOR_PATTERN = Pattern.compile(".");

	@Autowired
	private OrdinaryLambdaListParser ordinaryLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.LAMBDA;
	}

	@Override
	public LambdaStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // Closure Expander SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			throw new TypeErrorException("LAMBDA: PARAMETER-LIST must be a List. Got: " + first);
		}
		final ListStruct parameters = (ListStruct) first;

		final Environment lambdaEnvironment = new Environment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, lambdaEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(lambdaEnvironment::addDynamicBinding);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String className;
		if (javaClassNameDeclaration == null) {
			final LispNameDeclarationStruct lispNameDeclarationStruct = declare.getLispNameDeclarationStruct();
			final String lambdaClassName;
			if (lispNameDeclarationStruct == null) {
				lambdaClassName = "Lambda" + '_' + System.nanoTime();
			} else {
				final String name = lispNameDeclarationStruct.getClassName().replace('-', '_');
				final String realName = name.chars()
				                            .filter(Character::isJavaIdentifierPart)
				                            .mapToObj(e -> (char) e)
				                            .map(String::valueOf)
				                            .collect(Collectors.joining());
				lambdaClassName = realName + '_' + "Lambda" + '_' + System.nanoTime();
			}
			className = "jcl/" + lambdaClassName;
		} else {
			final String javaClassName = javaClassNameDeclaration.getClassName();
			if (javaClassName.startsWith(".") || javaClassName.endsWith(".")) {
				throw new ErrorException("Invalid class definition for Lambda: " + javaClassName);
			}

			final int lastSeparator = javaClassName.lastIndexOf('.');
			final String packageNameString = javaClassName.substring(0, lastSeparator);

			final boolean hasInvalidPackageName =
					!Arrays.stream(CLASS_SEPARATOR_PATTERN.split(packageNameString))
					       .map(CharSequence::chars)
					       .allMatch(packageStringChars -> packageStringChars.allMatch(Character::isJavaIdentifierPart));
			if (hasInvalidPackageName) {
				throw new ErrorException("Invalid package name for Lambda: " + packageNameString);
			}

			final String classNameString = javaClassName.substring(lastSeparator + 1);
			final boolean hasInvalidClassName = !Character.isJavaIdentifierStart(classNameString.charAt(0)) || !classNameString.chars().allMatch(Character::isJavaIdentifierPart);
			if (hasInvalidClassName) {
				throw new ErrorException("Invalid class name for Lambda: " + classNameString);
			}

			// TODO: Remove System.nanoTime() from here, since this breaks JAR loading. But we need it for now.
			className = javaClassNameDeclaration.getClassName().replace('.', '/') + '_' + System.nanoTime();
		}

		final OrdinaryLambdaList parsedLambdaList = ordinaryLambdaListParser.parseOrdinaryLambdaList(lambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, lambdaEnvironment))
				           .collect(Collectors.toList());
		return new LambdaStruct(className, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), lambdaEnvironment);
	}
}
