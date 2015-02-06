package jcl.compiler.real.sa.analyzer.specialoperator.lambda;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.ListStructAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class LambdaAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -7592502247452528911L;

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Autowired
	private ListStructAnalyzer listStructAnalyzer;

	@Override
	public LambdaElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LAMBDA: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final LambdaEnvironment lambdaEnvironment = new LambdaEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(lambdaEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct parameters = (ListStruct) second;

			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(analyzer, analysisBuilder, parameters, declareElement);
			final List<LispStruct> newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList);

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();
			newStartingLambdaBody.addAll(realBodyForms);

			final ListStruct newLambdaBodyListStruct = ListStruct.buildProperList(newStartingLambdaBody);

			// TODO: HOLY SHIT BUG: What's happening is that we're creating a new body here and trying to just analyze every element of it instead of delegating properly.
			// TODO:    Need to research this in ALL the other analyzers as well.
			final Element analyzedBodyForms = listStructAnalyzer.analyze(analyzer, newLambdaBodyListStruct, analysisBuilder);

			return new LambdaElement(parsedLambdaList, bodyProcessingResult.getDocString(), null, lambdaEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static List<LispStruct> getNewStartingLambdaBody(final OrdinaryLambdaListBindings parsedLambdaList) {
		final List<LispStruct> newLambdaBody = new ArrayList<>();

		final List<AuxBinding> auxBindings = parsedLambdaList.getAuxBindings();
		if (auxBindings.isEmpty()) {
			newLambdaBody.add(SpecialOperator.PROGN);
		} else {
			newLambdaBody.add(SpecialOperator.LET_STAR);

			final List<LispStruct> auxLetStarVars = auxBindings
					.stream()
					.map(e -> new ConsStruct(e.getSymbolStruct(), e.getInitForm()))
					.collect(Collectors.toList());

			final ListStruct auxLetStarVarsLL = ListStruct.buildProperList(auxLetStarVars);
			newLambdaBody.add(auxLetStarVarsLL);
		}

		final List<LispStruct> initFormIfSetqs = getInitFormIfSetqs(parsedLambdaList);
		newLambdaBody.addAll(initFormIfSetqs);

		return newLambdaBody;
	}

	private static List<LispStruct> getInitFormIfSetqs(final OrdinaryLambdaListBindings parsedLambdaList) {

		final List<LispStruct> initFormIfSetqs = new ArrayList<>();

		final List<OptionalBinding> optionalBindings = parsedLambdaList.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();

			final List<LispStruct> initFormIfSetq = new ArrayList<>();
			initFormIfSetq.add(SpecialOperator.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final List<LispStruct> initFormSetq = new ArrayList<>();
			initFormSetq.add(SpecialOperator.SETQ);
			initFormSetq.add(optionalBinding.getSymbolStruct());
			initFormSetq.add(optionalBinding.getInitForm());

			final ListStruct initFormSetqLL = ListStruct.buildProperList(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ListStruct initFormIfSetqLL = ListStruct.buildProperList(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		final List<KeyBinding> keyBindings = parsedLambdaList.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();

			final List<LispStruct> initFormIfSetq = new ArrayList<>();
			initFormIfSetq.add(SpecialOperator.IF);
			initFormIfSetq.add(suppliedPBinding.getSymbolStruct());

			final List<LispStruct> initFormSetq = new ArrayList<>();
			initFormSetq.add(SpecialOperator.SETQ);
			initFormSetq.add(keyBinding.getSymbolStruct());
			initFormSetq.add(keyBinding.getInitForm());

			final ListStruct initFormSetqLL = ListStruct.buildProperList(initFormSetq);
			initFormIfSetq.add(initFormSetqLL);

			final ListStruct initFormIfSetqLL = ListStruct.buildProperList(initFormIfSetq);
			initFormIfSetqs.add(initFormIfSetqLL);
		}

		return initFormIfSetqs;
	}
}
