package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.SetqStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqExpander extends MacroFunctionExpander<SetqStruct> {

	private static final long serialVersionUID = 5324580926862048137L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.SETQ;
	}

	@Override
	public SetqStruct expand(final ListStruct form, final Environment environment) {

		final ListStruct formRest = form.getRest();
		final int numberOfForms = validator.validateListFormSizeEven(formRest, "SETQ");

		final List<LispStruct> forms = formRest.getAsJavaList();

		final List<SetqStruct.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < numberOfForms; index += 2) {

			final LispStruct setqVar = forms.get(index);
			final SymbolStruct setqVarSymbol = validator.validateObjectType(setqVar, "SETQ", "VARIABLE", SymbolStruct.class);

			final LispStruct setqForm = forms.get(index + 1);
			final LispStruct setqFormAnalyzed = formAnalyzer.analyze(setqForm, environment);

			final SetqStruct.SetqPair setqPair = new SetqStruct.SetqPair(setqVarSymbol, setqFormAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqStruct(setqPairs);
	}
}
