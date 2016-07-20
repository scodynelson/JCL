package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.IfStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfExpander extends MacroFunctionExpander<IfStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.IF;
	}

	@Override
	public IfStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // IF SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: 0. Expected between 2 and 3 arguments.");
		}
		final LispStruct testForm = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: 1. Expected between 2 and 3 arguments.");
		}
		final LispStruct thenForm = iterator.next();

		LispStruct elseForm = null;
		if (iterator.hasNext()) {
			elseForm = iterator.next();
		}
		if (iterator.hasNext()) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: 4. Expected between 2 and 3 arguments.");
		}

		final LispStruct testFormAnalyzed = formAnalyzer.analyze(testForm, environment);
		final LispStruct thenFormAnalyzed = formAnalyzer.analyze(thenForm, environment);
		final LispStruct elseFormAnalyzed;
		if (elseForm == null) {
			elseFormAnalyzed = NILStruct.INSTANCE;
		} else {
			elseFormAnalyzed = formAnalyzer.analyze(elseForm, environment);
		}
		return new IfStruct(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
	}
}