package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jcl.compiler.StackUtils;
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
import jcl.lang.DeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.array.StringStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.list.ListStruct;
import jcl.type.TType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MacroletExpander extends MacroFunctionExpander<InnerLambdaStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(MacroletExpander.class);

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private FunctionExpander functionExpander;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.MACROLET;
	}

	@Override
	public InnerLambdaStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MACROLET SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			throw new TypeErrorException("MACROLET: MACRO-LIST must be a List. Got: " + first);
		}
		final ListStruct innerMacroLambdas = (ListStruct) first;

		final Environment macroletEnvironment = new Environment(environment);

		final Stack<SymbolStruct> functionNameStack = macroletEnvironment.getFunctionNameStack();
		final List<SymbolStruct> functionNames = getFunctionNames(innerMacroLambdas);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = LispStructFactory.toProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, macroletEnvironment);

		try {
			// Add function names BEFORE analyzing the functions. This is one of the differences between Flet and Labels/Macrolet.
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<InnerLambdaStruct.InnerLambdaVar> macroletVars
					= innerMacroLambdas.stream()
					                   .map(e -> getMacroletVar(e, declare, macroletEnvironment))
					                   .collect(Collectors.toList());

			final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
			specialDeclarations.stream()
			                   .map(SpecialDeclarationStruct::getVar)
			                   .map(Binding::new)
			                   .forEach(macroletEnvironment::addDynamicBinding);

			final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
			final List<LispStruct> analyzedBodyForms
					= bodyForms.stream()
					           .map(e -> formAnalyzer.analyze(e, macroletEnvironment))
					           .collect(Collectors.toList());

			return new InnerLambdaStruct(macroletVars, new PrognStruct(analyzedBodyForms), macroletEnvironment);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}
		}
	}

	private List<SymbolStruct> getFunctionNames(final ListStruct innerMacroLambdas) {

		final List<SymbolStruct> functionNames = new ArrayList<>();

		for (final LispStruct functionDefinition : innerMacroLambdas) {

			if (!(functionDefinition instanceof ListStruct)) {
				throw new ProgramErrorException("MACROLET: FUNCTION PARAMETER must be a List. Got: " + functionDefinition);
			}
			final ListStruct functionList = (ListStruct) functionDefinition;

			final LispStruct functionListFirst = functionList.getCar();
			if (!(functionListFirst instanceof SymbolStruct)) {
				throw new ProgramErrorException("MACROLET: First element of function parameter must be a Symbol. Got: " + functionListFirst);
			}
			final SymbolStruct functionName = (SymbolStruct) functionListFirst;

			if (functionNames.contains(functionName)) {
				LOGGER.warn("MACROLET: Multiple bindings of {} in MACROLET form.", functionName.getName());
			}
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private InnerLambdaStruct.InnerLambdaVar getMacroletVar(final LispStruct functionDefinition, final DeclareStruct declare,
	                                                        final Environment macroletEnvironment) {

		final ListStruct functionList = (ListStruct) functionDefinition;
		final SymbolStruct functionName = (SymbolStruct) functionList.getCar();
		final CompilerFunctionStruct functionInitForm = getFunctionParameterInitForm(functionList, macroletEnvironment);

		final boolean isSpecial = declare.getSpecialDeclarations()
		                                 .stream()
		                                 .map(SpecialDeclarationStruct::getVar)
		                                 .anyMatch(Predicate.isEqual(functionName));

		final Binding binding = new Binding(functionName, TType.INSTANCE);
		if (isSpecial) {
			macroletEnvironment.addDynamicBinding(binding);
		} else {
			macroletEnvironment.addLexicalBinding(binding);
		}

		return new InnerLambdaStruct.InnerLambdaVar(functionName, functionInitForm, isSpecial);
	}

	private CompilerFunctionStruct getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                            final Environment macroletEnvironment) {

		// TODO: This will be a MacroLambda, NOT a Lambda form!!!

		final Iterator<LispStruct> iterator = functionListParameter.iterator();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACROLET: Function parameter must have at least 2 elements. Got: 0");
		}
		final LispStruct functionName = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MACROLET: Function parameter must have at least 2 elements. Got: 1");
		}
		final LispStruct lambdaList = iterator.next();

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final List<LispStruct> declares = bodyProcessingResult.getDeclares();
		final StringStructImpl docString = bodyProcessingResult.getDocString();
		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		// NOTE: Make Dotted list here so the 'contents' of the body get added to the block
		final ListStruct blockBody = LispStructFactory.toProperList(bodyForms);
		final ListStruct innerBlockListStruct = LispStructFactory.toDottedList(SpecialOperatorStruct.BLOCK, functionName, blockBody);

		// NOTE: This will be a safe cast since we verify it is a symbol earlier
		final SymbolStruct functionNameSymbol = (SymbolStruct) functionName;

		final String functionNameString = functionNameSymbol.getName();
		final String properFunctionNameString = functionNameString.codePoints()
		                                                          .filter(Character::isJavaIdentifierPart)
		                                                          .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		                                                          .toString();

		final String macroletParamName = "jcl.MACROLET_" + properFunctionNameString + "_MacroLambda_" + System.nanoTime();
		final StringStructImpl macroletParamJavaClassName = LispStructFactory.toString(macroletParamName);
		final ListStruct macroletParamJavaClassNameDeclaration = LispStructFactory.toProperList(DeclarationStruct.JAVA_CLASS_NAME, macroletParamJavaClassName);
		declares.add(macroletParamJavaClassNameDeclaration);

		final ListStruct fullDeclaration = LispStructFactory.toProperList(declares);

		final ListStruct innerLambdaListStruct = LispStructFactory.toProperList(SpecialOperatorStruct.LAMBDA, lambdaList, fullDeclaration, docString, innerBlockListStruct);
		final ListStruct innerFunctionListStruct = LispStructFactory.toProperList(SpecialOperatorStruct.FUNCTION, innerLambdaListStruct);

		// Evaluate in the 'current' environment.
		return functionExpander.expand(innerFunctionListStruct, macroletEnvironment);
	}
}
