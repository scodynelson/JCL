package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ReturnFromStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromExpander extends MacroFunctionExpander<ReturnFromStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.RETURN_FROM;
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // RETURN-FROM SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: 0. Expected between 1 and 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new TypeErrorException("RETURN-FROM: NAME must be a Symbol. Got: " + first);
		}
		final SymbolStruct name = (SymbolStruct) first;

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
			result = formAnalyzer.analyze(third, environment);
		}
		return new ReturnFromStruct(name, result);
	}
}
