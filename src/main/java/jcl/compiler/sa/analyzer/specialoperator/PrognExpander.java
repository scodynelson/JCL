package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PrognExpander extends MacroFunctionExpander<PrognStruct> {

	public static final PrognExpander INSTANCE = new PrognExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.PROGN;
	}

	@Override
	public PrognStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // PROGN SYMBOL

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> FormAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());
		return new PrognStruct(analyzedForms);
	}
}
