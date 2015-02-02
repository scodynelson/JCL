package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.UUID;

@Component
public class LoadTimeValueAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 2) || (input.size() > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + input.size() + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof BooleanStruct)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be of type BooleanStruct. Got: " + third);
		}

		final List<LispStruct> lambdaBlock = new ArrayList<>();
		lambdaBlock.add(SpecialOperator.LAMBDA);
		lambdaBlock.add(NullStruct.INSTANCE);

		final LispStruct loadTimeValueForm = input.getRest().getFirst();
		lambdaBlock.add(loadTimeValueForm);

		final ListStruct lambdaBlockList = ListStruct.buildProperList(lambdaBlock);

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment nullLexicalEnvironment = LexicalEnvironment.NULL;
		lexicalEnvironmentStack.push(nullLexicalEnvironment);

		final LispStruct lambdaAnalyzed;
		try {
			lambdaAnalyzed = lambdaAnalyzer.analyze(analyzer, lambdaBlockList, analysisBuilder);
		} finally {
			lexicalEnvironmentStack.pop();
		}

		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();
		final LexicalEnvironment enclosingLambdaEnvironment = getEnclosingLambda(currentLexicalEnvironment);

		final String ltvNameString = "LOAD_TIME_VALUE" + UUID.randomUUID();
		final SymbolStruct<?> ltvName = new SymbolStruct<>(ltvNameString);

		// TODO: load-time-value read-only-p
		final LoadTimeValue newLoadTimeValue = new LoadTimeValue(ltvName, lambdaAnalyzed);
		enclosingLambdaEnvironment.getLoadTimeValues().add(newLoadTimeValue);

		return ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, ltvName);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param lexicalEnvironment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	private static LexicalEnvironment getEnclosingLambda(final LexicalEnvironment lexicalEnvironment) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
