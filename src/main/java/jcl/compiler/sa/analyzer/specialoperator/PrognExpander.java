package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PrognExpander extends MacroFunctionExpander<PrognStruct> {

	private static final long serialVersionUID = -2851059577992887882L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.PROGN;
	}

	@Override
	public PrognStruct expand(final ListStruct form, final Environment environment) {

		final ListStruct formRest = form.getRest();

		final List<LispStruct> forms = formRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new PrognStruct(analyzedForms);
	}
}
