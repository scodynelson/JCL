package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.AnalysisBuilder;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	private static final long serialVersionUID = 2755221428467421207L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.PROGV.setMacroFunctionExpander(this);
	}

	@Override
	public ProgvStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + inputSize + ". Expected at least 3 arguments.");
		}

		// Check Vars List

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Symbols list must be a quoted ListStruct. Got: " + second);
		}

		final ListStruct secondListStruct = (ListStruct) second;
		if (secondListStruct.size() != 2) {
			throw new ProgramErrorException("PROGV: Symbols list must be properly quoted: " + second);
		}
		if (!SpecialOperator.QUOTE.equals(secondListStruct.getFirst())) {
			throw new ProgramErrorException("PROGV: Symbols list must be quoted: " + second);
		}

		final LispStruct actualVarsList = secondListStruct.getRest().getFirst();
		if (!(actualVarsList instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: TODO: Symbols list must be of type ListStruct. Got: " + actualVarsList);
		}

		final ListStruct actualVarsListStruct = (ListStruct) actualVarsList;
		final List<? extends LispStruct> actualVarsJavaList = actualVarsListStruct.getAsJavaList();
		for (final LispStruct currentVar : actualVarsJavaList) {
			if (!(currentVar instanceof SymbolStruct)) {
				throw new ProgramErrorException("PROGV: Elements in symbols list must be of type SymbolStruct. Got: " + currentVar);
			}
		}

		// Check Vals List

		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct third = inputRestRest.getFirst();
		if (!(third instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be a quoted ListStruct. Got: " + third);
		}

		final ListStruct thirdListStruct = (ListStruct) third;
		if (thirdListStruct.size() != 2) {
			throw new ProgramErrorException("PROGV: Values list must be properly quoted: " + second);
		}
		if (!SpecialOperator.QUOTE.equals(thirdListStruct.getFirst())) {
			throw new ProgramErrorException("PROGV: Values list must be quoted: " + second);
		}

		final LispStruct actualValsList = thirdListStruct.getRest().getFirst();
		if (!(actualValsList instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be of type ListStruct. Got: " + actualValsList);
		}

		final ListStruct actualValsListStruct = (ListStruct) actualValsList;
		final List<? extends LispStruct> actualValsJavaList = actualValsListStruct.getAsJavaList();

		// Do other stuff

		final AnalysisBuilder analysisBuilder = environment.getAnalysisBuilder();
		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final int numberOfProgvVars = actualVarsJavaList.size();
		final List<ProgvStruct.ProgvVar> progvVars = new ArrayList<>(numberOfProgvVars);

		for (int i = 0; i < numberOfProgvVars; i++) {

			// NOTE: We can cast here since we checked the type earlier
			final SymbolStruct<?> var = (SymbolStruct) actualVarsJavaList.get(i);
			LispStruct val = NullStruct.INSTANCE;
			if (i < actualValsJavaList.size()) {
				val = actualValsJavaList.get(i);
			}

			final SymbolStruct<?> varSE = symbolAnalyzer.analyzeDynamic(var, currentEnvironment);

			final LispStruct analyzedVal = formAnalyzer.analyze(val, currentEnvironment);
			final ProgvStruct.ProgvVar progvVar = new ProgvStruct.ProgvVar(varSE, analyzedVal);

			// TODO: really a 'null' allocation here???
			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, null, T.INSTANCE, analyzedVal);

			progvVars.add(progvVar);
		}

		final List<LispStruct> forms = inputRestRest.getRest().getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, currentEnvironment))
				     .collect(Collectors.toList());

		return new ProgvStruct(progvVars, analyzedForms, null, currentEnvironment);
	}
}
