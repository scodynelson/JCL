package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.TheStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class TheExpander extends MacroFunctionExpander<TheStruct> {

	public static final TheExpander INSTANCE = new TheExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.THE;
	}

	@Override
	public TheStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // THE SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 0. Expected 2 arguments.");
		}
		final LispStruct valueType = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 1. Expected 2 arguments.");
		}
		final LispStruct theForm = iterator.next();

		if (!(theForm instanceof SymbolStruct) && !(theForm instanceof ListStruct)) {
			throw new TypeErrorException("THE: TYPE-SPECIFIER must be either a Symbol or a List. Got: " + theForm);
		}

		if (iterator.hasNext()) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: 3. Expected 2 arguments.");
		}

		final LispStruct theFormAnalyzed = FormAnalyzer.analyze(theForm, environment);
		return new TheStruct(null, theFormAnalyzed);
	}
}
