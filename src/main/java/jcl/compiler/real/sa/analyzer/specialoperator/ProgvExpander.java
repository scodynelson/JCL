package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
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
	 * Initializes the progv macro function and adds it to the special operator 'progv'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.PROGV.setMacroFunctionExpander(this);
	}

	@Override
	public ProgvStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + formSize + ". Expected at least 3 arguments.");
		}

		// Check Vars List
		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Symbols list must be a quoted list. Got: " + second);
		}

		final ListStruct secondAsList = (ListStruct) second;
		if (secondAsList.size() != 2) {
			throw new ProgramErrorException("PROGV: Symbols list must be properly quoted: " + second);
		}
		final LispStruct secondAsListFirst = secondAsList.getFirst();
		if (!SpecialOperator.QUOTE.equals(secondAsListFirst)) {
			throw new ProgramErrorException("PROGV: Symbols list must be quoted: " + second);
		}

		final ListStruct secondAsListRest = secondAsList.getRest();
		final LispStruct actualVars = secondAsListRest.getFirst();
		if (!(actualVars instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: TODO: Symbols list must be a list. Got: " + actualVars);
		}

		final ListStruct actualVarsAsList = (ListStruct) actualVars;
		final List<LispStruct> actualVarsAsJavaList = actualVarsAsList.getAsJavaList();
		for (final LispStruct currentVar : actualVarsAsJavaList) {
			if (!(currentVar instanceof SymbolStruct)) {
				throw new ProgramErrorException("PROGV: Elements in symbols list must be symbols. Got: " + currentVar);
			}
		}

		// Check Vals List
		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		if (!(third instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be a quoted list. Got: " + third);
		}

		final ListStruct thirdAsList = (ListStruct) third;
		if (thirdAsList.size() != 2) {
			throw new ProgramErrorException("PROGV: Values list must be properly quoted: " + second);
		}
		final LispStruct thirdAsListFirst = thirdAsList.getFirst();
		if (!SpecialOperator.QUOTE.equals(thirdAsListFirst)) {
			throw new ProgramErrorException("PROGV: Values list must be quoted: " + second);
		}

		final ListStruct thirdAsListRest = thirdAsList.getRest();
		final LispStruct actualVals = thirdAsListRest.getFirst();
		if (!(actualVals instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be a list. Got: " + actualVals);
		}

		final ListStruct actualValsAsList = (ListStruct) actualVals;
		final List<LispStruct> actualValsAsJavaList = actualValsAsList.getAsJavaList();

		// Handle Progn Environment processing
		final ProgvEnvironment progvEnvironment = new ProgvEnvironment(environment);

		final int numberOfProgvVars = actualVarsAsJavaList.size();
		final List<ProgvStruct.ProgvVar> progvVars = new ArrayList<>(numberOfProgvVars);

		for (int i = 0; i < numberOfProgvVars; i++) {

			// NOTE: We can safely cast here since we checked the type earlier
			final SymbolStruct<?> var = (SymbolStruct) actualVarsAsJavaList.get(i);

			final LispStruct val;
			if (i < actualValsAsJavaList.size()) {
				val = actualValsAsJavaList.get(i);
			} else {
				val = NullStruct.INSTANCE;
			}

			final SymbolStruct<?> analyzedVar = symbolAnalyzer.analyzeDynamic(var, progvEnvironment);
			final LispStruct analyzedVal = formAnalyzer.analyze(val, progvEnvironment);
			final ProgvStruct.ProgvVar progvVar = new ProgvStruct.ProgvVar(analyzedVar, analyzedVal);

			final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(progvEnvironment);
			final int nextParameterNumber = currentLambda.getNextParameterNumber();
			progvEnvironment.setBindingsPosition(nextParameterNumber);

			final ParameterAllocation allocation = new ParameterAllocation(nextParameterNumber);
			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, analyzedVal);
			progvEnvironment.addDynamicBinding(binding);

			progvVars.add(progvVar);
		}

		final ListStruct formRestRestRest = formRestRest.getRest();

		final List<LispStruct> bodyForms = formRestRestRest.getAsJavaList();
		final List<LispStruct> analyzedBodyForms =
				bodyForms.stream()
				         .map(e -> formAnalyzer.analyze(e, environment))
				         .collect(Collectors.toList());

		return new ProgvStruct(progvVars, analyzedBodyForms, null, environment);
	}
}
