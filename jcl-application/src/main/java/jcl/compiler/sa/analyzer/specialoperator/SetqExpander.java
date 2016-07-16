package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.SetqStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqExpander extends MacroFunctionExpander<SetqStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.SETQ;
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
				final String printedObject = printer.print(setqVar);
				throw new TypeErrorException("SETQ: VARIABLE must be a Symbol. Got: " + printedObject);
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
