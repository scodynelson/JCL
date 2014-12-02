package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.lambdalist.AuxBinding;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.LambdaEnvironmentLispStruct;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAndDocStringAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
public class LambdaAnalyzer implements Analyzer<LambdaEnvironmentLispStruct, ListStruct> {

	@Autowired
	private BodyWithDeclaresAndDocStringAnalyzer bodyWithDeclaresAndDocStringAnalyzer;

	@Override
	public LambdaEnvironmentLispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LAMBDA: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LAMBDA: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment lambdaEnvironment = new Environment(parentEnvironment, Marker.LAMBDA, newClosureDepth);
		environmentStack.push(lambdaEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct parameters = (ListStruct) second;
			final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(analyzer, analysisBuilder, parameters);

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAndDocStringAnalyzer.analyze(analyzer, currentBodyForms, analysisBuilder);

			final Environment envList = environmentStack.peek();

			final List<LispStruct> newStartingLambdaBody = getNewStartingLambdaBody(parsedLambdaList);
			newStartingLambdaBody.addAll(bodyProcessingResult.getBodyForms());

			final ListStruct newBodyForms = ListStruct.buildProperList(newStartingLambdaBody);
			return new LambdaEnvironmentLispStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms, parsedLambdaList, bodyProcessingResult.getDocString());
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
