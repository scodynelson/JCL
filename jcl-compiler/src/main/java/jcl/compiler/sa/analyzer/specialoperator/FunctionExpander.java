package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.analyzer.LambdaExpander;
import jcl.compiler.sa.analyzer.MacroLambdaExpander;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.MacroLambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.compiler.struct.specialoperator.lambda.MacroLambdaStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class FunctionExpander extends MacroFunctionExpander<CompilerFunctionStruct> {

	public static final FunctionExpander INSTANCE = new FunctionExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.FUNCTION;
	}

	@Override
	public CompilerFunctionStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // FUNCTION SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: 0. Expected 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (iterator.hasNext()) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: 3. Expected 2 arguments.");
		}

		if (first instanceof SymbolStruct) {
			return new SymbolCompilerFunctionStruct((SymbolStruct) first);
		} else if (first instanceof ListStruct) {
			return analyzeFunctionList((ListStruct) first, environment);
		} else {
			throw new TypeErrorException("FUNCTION: FUNCTION-ARGUMENT must be a Symbol or a List. Got: " + first);
		}
	}

	private CompilerFunctionStruct analyzeFunctionList(final ListStruct functionList, final Environment environment) {

		final LispStruct functionListFirst = functionList.car();

		if (SpecialOperatorStructImpl.LAMBDA.eq(functionListFirst)) {
			final LambdaStruct analyzedLambda = LambdaExpander.INSTANCE.expand(functionList, environment);
			return new LambdaCompilerFunctionStruct(analyzedLambda);
		} else if(!SpecialOperatorStructImpl.LAMBDA.eq(functionListFirst)) {
			final MacroLambdaStruct analyzedMacroLambda = MacroLambdaExpander.INSTANCE.expand(functionList, environment);
			return new MacroLambdaCompilerFunctionStruct(analyzedMacroLambda);
		} else {
			throw new ProgramErrorException("FUNCTION: First element of list argument must be the symbol 'LAMBDA'. Got: " + functionListFirst);
		}
	}
}
