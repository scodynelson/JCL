package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
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
	private Printer printer;

	/**
	 * Initializes the setq macro function and adds it to the special operator 'setq'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.SETQ.setMacroFunctionExpander(this);
	}

	@Override
	public SetqStruct expand(final ListStruct form, final Environment environment) {

		final ListStruct formRest = form.getRest();
		final List<LispStruct> forms = formRest.getAsJavaList();

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			final String printedObject = printer.print(formRest);
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + printedObject + ". Expected an even number of arguments.");
		}

		final List<SetqStruct.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < numberOfForms; index += 2) {

			final LispStruct setqVar = forms.get(index);
			if (!(setqVar instanceof SymbolStruct)) {
				final String printedSetqVar = printer.print(setqVar);
				throw new ProgramErrorException("SETQ: Variable must be a symbol. Got: " + printedSetqVar);
			}
			final SymbolStruct<?> setqVarSymbol = (SymbolStruct<?>) setqVar;

			final LispStruct setqForm = forms.get(index + 1);
			final LispStruct setqFormAnalyzed = formAnalyzer.analyze(setqForm, environment);

			final SetqStruct.SetqPair setqPair = new SetqStruct.SetqPair(setqVarSymbol, setqFormAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqStruct(setqPairs);
	}
}
