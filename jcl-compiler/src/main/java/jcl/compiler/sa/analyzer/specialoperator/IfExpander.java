package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.IfStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class IfExpander extends MacroFunctionExpander<IfStruct> {

	public static final IfExpander INSTANCE = new IfExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.IF;
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

		final LispStruct testFormAnalyzed = FormAnalyzer.analyze(testForm, environment);
		final LispStruct thenFormAnalyzed = FormAnalyzer.analyze(thenForm, environment);
		final LispStruct elseFormAnalyzed;
		if (elseForm == null) {
			elseFormAnalyzed = NILStruct.INSTANCE;
		} else {
			elseFormAnalyzed = FormAnalyzer.analyze(elseForm, environment);
		}
		return new IfStruct(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
	}
}
