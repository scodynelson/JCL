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
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.functions.EvalFunction;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	private static final long serialVersionUID = 2755221428467421207L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the progv macro function and adds it to the special operator 'progv'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.PROGV.setMacroFunctionExpander(this);
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
		// TODO: This eval needs to happen dynamically and let the CG take care of it.
		final LispStruct vars = evalFunction.eval(second, environment);
		if (!(vars instanceof ListStruct)) {
			final String printedObject = printer.print(vars);
			throw new ProgramErrorException("PROGV: Symbols list must be a list. Got: " + printedObject);
		}

		final ListStruct varsAsList = (ListStruct) vars;
		final List<LispStruct> varsAsJavaList = varsAsList.getAsJavaList();
		for (final LispStruct currentVar : varsAsJavaList) {
			if (!(currentVar instanceof SymbolStruct)) {
				final String printedObject = printer.print(currentVar);
				throw new ProgramErrorException("PROGV: Elements in symbols list must be symbols. Got: " + printedObject);
			}
		}

		// Check Vals List
		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		// TODO: This eval needs to happen dynamically and let the CG take care of it.
		final LispStruct vals = evalFunction.eval(third, environment);
		if (!(vals instanceof ListStruct)) {
			final String printedObject = printer.print(vals);
			throw new ProgramErrorException("PROGV: Values list must be a list. Got: " + printedObject);
		}

		final ListStruct valsAsList = (ListStruct) vals;
		final List<LispStruct> valsAsJavaList = valsAsList.getAsJavaList();

		// Handle Progn Environment processing
		final ProgvEnvironment progvEnvironment = new ProgvEnvironment(environment);

		final int numberOfProgvVars = varsAsJavaList.size();
		final List<ProgvStruct.ProgvVar> progvVars = new ArrayList<>(numberOfProgvVars);

		for (int i = 0; i < numberOfProgvVars; i++) {

			// NOTE: We can safely cast here since we checked the type earlier
			final SymbolStruct<?> var = (SymbolStruct) varsAsJavaList.get(i);

			final LispStruct val;
			if (i < valsAsJavaList.size()) {
				val = valsAsJavaList.get(i);
			} else {
				// TODO: These need to be bound to "no value". Hmm....
				val = NullStruct.INSTANCE;
			}

			final SymbolStruct<?> analyzedVar = symbolAnalyzer.analyzeDynamic(var, progvEnvironment);
			final LispStruct analyzedVal = formAnalyzer.analyze(val, progvEnvironment);
			final ProgvStruct.ProgvVar progvVar = new ProgvStruct.ProgvVar(analyzedVar, analyzedVal);

			final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(progvEnvironment);
			final int nextParameterNumber = currentLambda.getNextParameterNumber();
			progvEnvironment.setBindingsPosition(nextParameterNumber);

			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, TType.INSTANCE, analyzedVal);
			progvEnvironment.addDynamicBinding(binding);

			progvVars.add(progvVar);
		}

		final ListStruct formRestRestRest = formRestRest.getRest();

		final List<LispStruct> bodyForms = formRestRestRest.getAsJavaList();
		final List<LispStruct> analyzedBodyForms =
				bodyForms.stream()
				         .map(e -> formAnalyzer.analyze(e, environment))
				         .collect(Collectors.toList());

		return new ProgvStruct(progvVars, new PrognStruct(analyzedBodyForms), progvEnvironment);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(symbolAnalyzer)
		                            .append(evalFunction)
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ProgvExpander rhs = (ProgvExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(symbolAnalyzer, rhs.symbolAnalyzer)
		                          .append(evalFunction, rhs.evalFunction)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(symbolAnalyzer)
		                                                                .append(evalFunction)
		                                                                .append(printer)
		                                                                .toString();
	}
}
