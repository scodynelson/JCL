package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.UUID;

public class LoadTimeValueAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final LoadTimeValueAnalyzer INSTANCE = new LoadTimeValueAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

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

		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		final Environment nullEnvironment = Environment.NULL;
		environmentStack.push(nullEnvironment);

		final LispStruct lambdaAnalyzed;
		try {
			lambdaAnalyzed = LambdaAnalyzer.INSTANCE.analyze(lambdaBlockList, analyzer);
		} finally {
			environmentStack.pop();
		}

		final Environment currentEnvironment = environmentStack.peek();
		final Environment enclosingLambda = EnvironmentAccessor.getEnclosingLambda(currentEnvironment);

		final String ltvNameString = "LOAD_TIME_VALUE" + UUID.randomUUID();
		final SymbolStruct<?> ltvName = new SymbolStruct<>(ltvNameString);

		// TODO: load-time-value read-only-p
		final LoadTimeValue newLoadTimeValue = new LoadTimeValue(ltvName, lambdaAnalyzed);
		enclosingLambda.getLoadTimeValues().add(newLoadTimeValue);

		return ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, ltvName);
	}
}
