package jcl.compiler.real.sa.analyzer;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.sa.analyzer.lambdalistparser.OrdinaryLambdaListParser;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaExpander extends MacroFunctionExpander<LambdaStruct> {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private OrdinaryLambdaListParser ordinaryLambdaListParser;

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
		return SpecialOperatorStruct.LAMBDA;
	}

	@Override
	public LambdaStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "LAMBDA");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct parameters = validator.validateObjectType(second, "LAMBDA", "PARAMETER LIST", ListStruct.class);

		final Environment lambdaEnvironment = new Environment(environment);

		final ListStruct formRestRest = formRest.getRest();
		final List<LispStruct> forms = formRestRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, lambdaEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(e -> new Binding(e, TType.INSTANCE))
		                   .forEach(lambdaEnvironment::addDynamicBinding);

		final JavaClassNameDeclarationStruct javaClassNameDeclaration = declare.getJavaClassNameDeclaration();
		final String className;
		if (javaClassNameDeclaration == null) {
			final String lambdaClassName = "Lambda" + '_' + System.nanoTime();
			className = "jcl/" + lambdaClassName;
		} else {
			className = javaClassNameDeclaration.getClassName().replace('.', '/');
		}

		final OrdinaryLambdaListBindings parsedLambdaList = ordinaryLambdaListParser.parseOrdinaryLambdaList(lambdaEnvironment, parameters, declare);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();
		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, lambdaEnvironment))
				           .collect(Collectors.toList());
		return new LambdaStruct(className, parsedLambdaList, bodyProcessingResult.getDocString(), new PrognStruct(analyzedBodyForms), lambdaEnvironment);
	}
}
