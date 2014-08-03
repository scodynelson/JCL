package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.Declaration;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class LoadTimeValueAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LoadTimeValueAnalyzer INSTANCE = new LoadTimeValueAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return analyze(input, "LOAD_TIME_VALUE");
	}

	public LispStruct analyze(final ListStruct input, final String ltvFieldName) {
		return analyze(input, ltvFieldName, UUID.randomUUID().toString());
	}

	private LispStruct analyze(final ListStruct input, final String ltvFieldName, final String tag) {

		final List<LispStruct> javaClassDeclaration = new ArrayList<>();
		javaClassDeclaration.add(Declaration.JAVA_CLASS_NAME);

		final SymbolStruct<?> name = new SymbolStruct<>(ltvFieldName + "_FN_" + tag);
		javaClassDeclaration.add(name);

		final ListStruct javaClassDeclarationList = ListStruct.buildProperList(javaClassDeclaration);

		final List<LispStruct> declarations = new ArrayList<>();
		declarations.add(SpecialOperator.DECLARE);
		declarations.add(javaClassDeclarationList);

		final ListStruct declarationsList = ListStruct.buildProperList(declarations);

		final List<LispStruct> lambdaBlock = new ArrayList<>();
		lambdaBlock.add(SpecialOperator.LAMBDA);
		lambdaBlock.add(NullStruct.INSTANCE);
		lambdaBlock.add(declarationsList);

		final ListStruct loadTimeValueBody = input.getRest();
		lambdaBlock.add(loadTimeValueBody);

		final ListStruct lambdaBlockList = ListStruct.buildProperList(lambdaBlock);

		final ListStruct lambdaAnalyzed;
		try {
			final Environment globalEnvironment = SemanticAnalyzer.environmentStack.elementAt(0);
			SemanticAnalyzer.environmentStack.push(globalEnvironment);
			lambdaAnalyzed = LambdaAnalyzer.INSTANCE.analyze(lambdaBlockList);
		} finally {
			SemanticAnalyzer.environmentStack.pop();
		}

		final Environment currentEnvironment = SemanticAnalyzer.environmentStack.peek();
		final Environment enclosingLambda = EnvironmentAccessor.getEnclosingLambda(currentEnvironment);

		final SymbolStruct<?> ltvName = new SymbolStruct<>(ltvFieldName + tag);

		final LoadTimeValue newLoadTimeValue = new LoadTimeValue(ltvName, lambdaAnalyzed);
		enclosingLambda.getLoadTimeValues().add(newLoadTimeValue);

		return ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, ltvName);
	}
}
