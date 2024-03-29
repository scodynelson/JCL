package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ReturnFromStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ReturnFromExpander extends MacroFunctionExpander<ReturnFromStruct> {

	public static final ReturnFromExpander INSTANCE = new ReturnFromExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.RETURN_FROM;
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // RETURN-FROM SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: 0. Expected between 1 and 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof final SymbolStruct name)) {
			throw new TypeErrorException("RETURN-FROM: NAME must be a Symbol. Got: " + first);
		}

		if (environment.getBlockStack().search(name) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + name + " is visible.");
		}

		LispStruct third = null;
		if (iterator.hasNext()) {
			third = iterator.next();
		}
		if (iterator.hasNext()) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: 2. Expected between 1 and 2 arguments.");
		}

		final LispStruct result;
		if (third == null) {
			result = NILStruct.INSTANCE;
		} else {
			result = FormAnalyzer.analyze(third, environment);
		}
		return new ReturnFromStruct(name, result);
	}
}
