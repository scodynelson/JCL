package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.SetqStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.SpecialOperatorStructImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqExpander extends MacroFunctionExpander<SetqStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.SETQ;
	}

	@Override
	public SetqStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // SETQ SYMBOL

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + numberOfForms + ". Expected an even number of arguments.");
		}

		final List<SetqStruct.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < numberOfForms; index += 2) {

			final LispStruct setqVar = forms.get(index);
			if (!(setqVar instanceof SymbolStruct)) {
				throw new TypeErrorException("SETQ: VARIABLE must be a Symbol. Got: " + setqVar);
			}
			final SymbolStruct setqVarSymbol = (SymbolStruct) setqVar;

			final LispStruct setqForm = forms.get(index + 1);
			final LispStruct setqFormAnalyzed = formAnalyzer.analyze(setqForm, environment);

			final SetqStruct.SetqPair setqPair = new SetqStruct.SetqPair(setqVarSymbol, setqFormAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqStruct(setqPairs);
	}
}
