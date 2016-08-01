package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ThrowStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ThrowExpander extends MacroFunctionExpander<ThrowStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.THROW;
	}

	@Override
	public ThrowStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // GO SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: 0. Expected 2 arguments.");
		}
		final LispStruct catchTag = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: 1. Expected 2 arguments.");
		}
		final LispStruct resultForm = iterator.next();

		if (iterator.hasNext()) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: 3. Expected 2 arguments.");
		}

		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);
		final LispStruct resultFormAnalyzed = formAnalyzer.analyze(resultForm, environment);
		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
